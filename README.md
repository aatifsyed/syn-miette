<!-- cargo-rdme start -->

A [`syn::Error`] wrapper that provides pretty diagnostic messages using [`miette`].

# Usage
```rust
let source = r"
pub struct {
    num_yaks: usize
}";

let error = syn::parse_str::<syn::DeriveInput>(source).unwrap_err();
let error = syn_miette::Error::new(error, source);

assert_eq!(
    error.render(), // only with `--feature render`
"  × expected identifier
   ╭─[2:12]
 1 │
 2 │ pub struct {
   ·            ┬
   ·            ╰── expected identifier
 3 │     num_yaks: usize
   ╰────
"
);
```


Notably, [`Error`](https://docs.rs/syn-miette/latest/syn_miette/struct.Error.html) properly renders children that have been [`syn::Error::combine`]-ed:
```text
  × duplicate definition of `Foo`
   ╭─[1:8]
 1 │ struct Foo;
   ·        ─┬─
   ·         ╰── initial definition here
 2 │ enum Bar {}
 3 │ union Foo {}
   ·       ─┬─
   ·        ╰── duplicate definition of `Foo`
   ╰────
```

<!-- cargo-rdme end -->
