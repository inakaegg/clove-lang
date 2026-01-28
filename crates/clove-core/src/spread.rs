pub fn strip_spread_symbol(name: &str) -> Option<String> {
    if name.len() < 2 {
        return None;
    }
    if !name.starts_with('*') {
        return None;
    }
    if name.ends_with('*') {
        return None;
    }
    if name == "*?" {
        return None;
    }
    if let Some(rest) = name.strip_prefix('*') {
        if !rest.is_empty() && rest.chars().all(|ch| ch.is_ascii_digit()) {
            return None;
        }
    }
    if let Some(rest) = name.strip_prefix("*?") {
        if !rest.is_empty() && rest.chars().all(|ch| ch.is_ascii_digit()) {
            return None;
        }
    }
    Some(name[1..].to_string())
}

pub fn is_spread_symbol(name: &str) -> bool {
    strip_spread_symbol(name).is_some()
}
