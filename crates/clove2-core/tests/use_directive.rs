use clove2_core::reader::read_all;
use clove2_core::use_directive::{parse_use_directives, Mode, NativeLevel};

#[test]
fn parse_use_directives_basic() {
    let forms = read_all("(use mode :native)\n(use imut-default true)\n(def x 1)").unwrap();
    let (cfg, consumed) = parse_use_directives(&forms).unwrap();
    assert_eq!(consumed, 2);
    assert_eq!(cfg.mode, Some(Mode::Native));
    assert_eq!(cfg.imut_default, Some(true));
}

#[test]
fn parse_use_native_level() {
    let forms = read_all("(use native :warn)").unwrap();
    let (cfg, consumed) = parse_use_directives(&forms).unwrap();
    assert_eq!(consumed, 1);
    assert_eq!(cfg.native_level, Some(NativeLevel::Warn));
}

#[test]
fn parse_use_mut_default() {
    let forms = read_all("(use mut-default true)").unwrap();
    let (cfg, consumed) = parse_use_directives(&forms).unwrap();
    assert_eq!(consumed, 1);
    assert_eq!(cfg.imut_default, Some(false));
}

#[test]
fn parse_use_mut_hard() {
    let forms = read_all("(use mut-hard true)").unwrap();
    let (cfg, consumed) = parse_use_directives(&forms).unwrap();
    assert_eq!(consumed, 1);
    assert_eq!(cfg.mut_hard, Some(true));
}
