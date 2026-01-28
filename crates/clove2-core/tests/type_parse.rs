use clove2_core::types::Type;

#[test]
fn parse_primitives() {
    assert_eq!(Type::parse("Int").unwrap(), Type::Int);
    assert_eq!(Type::parse("Str").unwrap(), Type::Str);
    assert_eq!(Type::parse("Dyn").unwrap(), Type::Dyn);
    assert_eq!(
        Type::parse("Config").unwrap(),
        Type::Named("Config".to_string())
    );
    assert_eq!(
        Type::parse("Str?").unwrap(),
        Type::Union(vec![Type::Str, Type::Nil])
    );
}

#[test]
fn parse_generics_and_union() {
    assert_eq!(
        Type::parse("Vec<Int>").unwrap(),
        Type::Vec(Box::new(Type::Int))
    );
    assert_eq!(
        Type::parse("Map<Str, Int>").unwrap(),
        Type::Map(Box::new(Type::Str), Box::new(Type::Int))
    );
    assert_eq!(
        Type::parse("Dyn<Int>").unwrap(),
        Type::DynOf(Box::new(Type::Int))
    );
    assert_eq!(
        Type::parse("Str|Nil").unwrap(),
        Type::Union(vec![Type::Str, Type::Nil])
    );
    assert_eq!(
        Type::parse("Map<Key, Dyn>").unwrap(),
        Type::Map(
            Box::new(Type::Union(vec![Type::Str, Type::Keyword])),
            Box::new(Type::Dyn)
        )
    );
    assert_eq!(
        Type::parse("[Int Str] -> Bool").unwrap(),
        Type::Function {
            params: vec![Type::Int, Type::Str],
            rest: None,
            ret: Box::new(Type::Bool),
        }
    );
    assert_eq!(
        Type::parse("[Int & Vec<Int>] -> Int").unwrap(),
        Type::Function {
            params: vec![Type::Int],
            rest: Some(Box::new(Type::Vec(Box::new(Type::Int)))),
            ret: Box::new(Type::Int),
        }
    );
}

#[test]
fn parse_object() {
    let ty = Type::parse("Object{port: Int, host: Str}").unwrap();
    match ty {
        Type::Object(fields) => {
            assert_eq!(fields.get("host"), Some(&Type::Str));
            assert_eq!(fields.get("port"), Some(&Type::Int));
        }
        other => panic!("expected object, got {:?}", other),
    }
}
