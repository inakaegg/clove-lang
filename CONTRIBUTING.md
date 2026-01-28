# Contributing

Thank you for contributing to Clove. Please read the following documents first:

- Development environment: `docs/contributing/dev_setup.md`
- Testing policy: `docs/contributing/testing.md`
- Repository layout: `docs/contributing/repo_layout.md`
- Documentation style: `docs/contributing/docs_style.md`

## Discussion and proposals

- For new specs or larger changes, open an Issue first to discuss.
- Small improvements or typo fixes are welcome as direct PRs.

## Change flow

1. Create a branch and make changes
2. Update related docs and examples
3. Run formatting and tests
4. Open a PR (include intent and verification details)

## Formatting and tests

- Rust:
  - `cargo fmt --all`
  - `cargo clippy --workspace --all-targets --all-features -D warnings`
  - `cargo test --workspace`
- Clove code examples:
  - Use `clove fmt --stdin` and follow `clovefmt.toml`.

## License

This project is dual-licensed under **MIT OR Apache-2.0**.
Unless explicitly stated otherwise, any intentional contribution is provided
under the same terms.
