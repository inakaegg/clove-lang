pub fn escape_string_fragment(text: &str) -> String {
    let mut escaped = format!("{:?}", text);
    if escaped.starts_with('"') && escaped.ends_with('"') && escaped.len() >= 2 {
        escaped = escaped[1..escaped.len() - 1].to_string();
    }
    escaped.replace("#{", "\\#{")
}
