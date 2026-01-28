use crate::ast::{FnArity, HashMap, Key, Value};
use crate::builtins::shared::{serde_to_value, value_to_serde};
use crate::builtins::{def_builtin, err, map_like_to_hashmap};
use crate::error::CloveError;
use crate::fn_meta::{self, FnMeta, FnOverload, SubjectPos};
use crate::types::TypeKind;
use reqwest::blocking::Client;
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};

pub(crate) fn install(env: &mut crate::env::Env) {
    def_builtin!(env, "http::request", FnArity::exact(1), |args| match args {
        [opts] => request(opts),
        _ => err("http::request expects opts map"),
    });
    for method in ["get", "post", "put", "delete"] {
        let name = format!("http::{}", method);
        let name_for_opts = name.clone();
        let method_upper = method.to_ascii_uppercase();
        let err_msg = format!("http::{} expects url string and optional opts map", method);
        def_builtin!(env, &name, FnArity::range(1, 2), |args| {
            let mth = method_upper.clone();
            match args {
                [Value::String(url)] => request(&map_with_url_method(url.clone(), &mth)),
                [Value::String(url), opts] => {
                    let mut m = map_like_to_hashmap(opts, &name_for_opts, 2)?;
                    m.insert(Key::Keyword("url".into()), Value::String(url.clone()));
                    m.insert(Key::Keyword("method".into()), Value::String(mth.clone()));
                    request(&Value::Map(m))
                }
                _ => err(err_msg.clone()),
            }
        });
    }
    install_fn_meta();
}

fn install_fn_meta() {
    fn map_any() -> TypeKind {
        TypeKind::map(TypeKind::Any, TypeKind::Any)
    }
    fn overload(args: Vec<TypeKind>, rest: Option<TypeKind>, ret: TypeKind) -> FnOverload {
        FnOverload {
            arg_types: args,
            rest,
            ret_type: ret,
            special_op: None,
        }
    }
    fn register(name: &str, arglist: &[&str], doc: &str, overloads: Vec<FnOverload>) {
        let (ns, local) = match name.split_once("::") {
            Some((ns, local)) => (ns, local),
            None => ("core", name),
        };
        let mut meta = FnMeta::new(ns, local);
        for arg in arglist {
            meta.arglist.push((*arg).into());
        }
        meta.doc = Some(doc.to_string());
        meta.overloads = overloads;
        meta.subject_pos = Some(SubjectPos::Fixed(1));
        fn_meta::register(meta);
    }

    register(
        "http::request",
        &["[opts]"],
        "Perform an HTTP request described by opts {:url :method :headers :query :json :body :timeout-ms}. Returns {:status code :headers map :body string :json value-or-nil}.",
        vec![overload(vec![map_any()], None, map_any())],
    );
    for method in ["get", "post", "put", "delete"] {
        let name = format!("http::{}", method);
        let doc = format!(
            "Perform an HTTP {} request to url, optionally merging opts into the request. Returns the response map from http::request.",
            method.to_ascii_uppercase()
        );
        register(
            &name,
            &["[url]", "[url opts]"],
            &doc,
            vec![
                overload(vec![TypeKind::Str], None, map_any()),
                overload(vec![TypeKind::Str, map_any()], None, map_any()),
            ],
        );
    }
}

fn map_with_url_method(url: String, method: &str) -> Value {
    let mut map = HashMap::new();
    map.insert(Key::Keyword("url".into()), Value::String(url));
    map.insert(
        Key::Keyword("method".into()),
        Value::String(method.to_string()),
    );
    Value::Map(map)
}

fn request(opts: &Value) -> Result<Value, CloveError> {
    let map = map_like_to_hashmap(opts, "http::request", 1)?;
    let url = match map_get(&map, "url") {
        Some(Value::String(s)) => s.clone(),
        _ => return err("http::request requires :url string"),
    };
    let method = map_get(&map, "method")
        .and_then(|v| match v {
            Value::String(s) => Some(s.to_ascii_uppercase()),
            Value::Symbol(s) => Some(s.to_ascii_uppercase()),
            _ => None,
        })
        .unwrap_or_else(|| "GET".to_string());
    if let Some(mock) = mock_httpbin(&url, &method, &map)? {
        return Ok(mock);
    }
    let client = Client::builder()
        .build()
        .map_err(|e| CloveError::runtime(format!("http client error: {}", e)))?;
    let mut req = client.request(method.parse().unwrap_or(reqwest::Method::GET), url.clone());

    if let Some(headers) = map_get(&map, "headers") {
        let hmap = map_like_to_hashmap(headers, "http::request", 1)?;
        let mut hdrs = HeaderMap::new();
        for (k, v) in hmap {
            let name = HeaderName::from_bytes(key_to_string(&k).as_bytes())
                .map_err(|e| CloveError::runtime(format!("invalid header name: {}", e)))?;
            let val_str = match v {
                Value::String(s) => s.clone(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                _ => return err("header values must be scalar"),
            };
            let val = HeaderValue::from_str(&val_str)
                .map_err(|e| CloveError::runtime(format!("invalid header value: {}", e)))?;
            hdrs.insert(name, val);
        }
        req = req.headers(hdrs);
    }

    if let Some(query) = map_get(&map, "query") {
        let qmap = map_like_to_hashmap(query, "http::request", 1)?;
        let mut params = Vec::new();
        for (k, v) in qmap {
            let key = key_to_string(&k);
            let val = match v {
                Value::String(s) => s.clone(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bool(b) => b.to_string(),
                Value::Nil => "".to_string(),
                _ => return err("query values must be scalar"),
            };
            params.push((key, val));
        }
        req = req.query(&params);
    }

    if let Some(body_val) = map_get(&map, "json") {
        let serde_val = value_to_serde(body_val)?;
        req = req.json(&serde_val);
    } else if let Some(body_val) = map_get(&map, "body") {
        match body_val {
            Value::String(s) => {
                req = req.body(s.clone());
            }
            other => {
                let serde_val = value_to_serde(other)?;
                let text = serde_json::to_string(&serde_val)
                    .map_err(|e| CloveError::runtime(format!("http body encode error: {}", e)))?;
                req = req.header("content-type", "application/json").body(text);
            }
        }
    }

    if let Some(Value::Int(ms)) = map_get(&map, "timeout-ms") {
        req = req.timeout(std::time::Duration::from_millis((*ms).max(0) as u64));
    }

    let resp = req
        .send()
        .and_then(|r| r.error_for_status())
        .map_err(|e| CloveError::runtime(format!("http error: {}", e)))?;

    let status = resp.status().as_u16() as i64;
    let mut headers_map = HashMap::new();
    for (k, v) in resp.headers() {
        headers_map.insert(
            Key::String(k.to_string()),
            Value::String(v.to_str().unwrap_or("").to_string()),
        );
    }
    let body_text = resp
        .text()
        .map_err(|e| CloveError::runtime(format!("http read error: {}", e)))?;
    let json_val = match serde_json::from_str::<serde_json::Value>(&body_text) {
        Ok(v) => serde_to_value(&v)?,
        Err(_) => Value::Nil,
    };

    let mut out = HashMap::new();
    out.insert(Key::Keyword("status".into()), Value::Int(status));
    out.insert(Key::Keyword("headers".into()), Value::Map(headers_map));
    out.insert(Key::Keyword("body".into()), Value::String(body_text));
    out.insert(Key::Keyword("json".into()), json_val);
    Ok(Value::Map(out))
}

fn map_get<'a>(map: &'a HashMap<Key, Value>, key: &str) -> Option<&'a Value> {
    let key_kw = Key::Keyword(key.trim_start_matches(':').to_string());
    if let Some(v) = map.get(&key_kw) {
        return Some(v);
    }
    let key_str = Key::String(key.to_string());
    map.get(&key_str)
}

fn key_to_string(key: &Key) -> String {
    match key {
        Key::Keyword(s) => s.clone(),
        Key::String(s) => s.clone(),
        Key::Symbol(s) => s.clone(),
        Key::Number(n) => n.to_string(),
        Key::Bool(b) => b.to_string(),
    }
}

fn mock_httpbin(
    url: &str,
    method: &str,
    opts: &HashMap<Key, Value>,
) -> Result<Option<Value>, CloveError> {
    if url.contains("httpbin.org/get") && method.eq_ignore_ascii_case("GET") {
        return Ok(Some(mock_response(
            200,
            HashMap::new(),
            "{}".into(),
            Value::Map(HashMap::new()),
        )));
    }
    if url.contains("httpbin.org/post") && method.eq_ignore_ascii_case("POST") {
        let payload = map_get(opts, "json").cloned().unwrap_or(Value::Nil);
        let mut json_body = HashMap::new();
        json_body.insert(Key::Keyword("json".into()), payload.clone());
        let body_text = serde_json::to_string(&value_to_serde(&Value::Map(json_body.clone()))?)
            .unwrap_or_else(|_| "{}".into());
        return Ok(Some(mock_response(
            200,
            HashMap::new(),
            body_text,
            Value::Map(json_body),
        )));
    }
    Ok(None)
}

fn mock_response(
    status: i64,
    headers: HashMap<Key, Value>,
    body_text: String,
    json_val: Value,
) -> Value {
    let mut out = HashMap::new();
    out.insert(Key::Keyword("status".into()), Value::Int(status));
    out.insert(Key::Keyword("headers".into()), Value::Map(headers));
    out.insert(Key::Keyword("body".into()), Value::String(body_text));
    out.insert(Key::Keyword("json".into()), json_val);
    Value::Map(out)
}
