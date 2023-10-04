<!-- cargo-rdme start -->

A [`syn::Error`] wrapper that provides pretty diagnostic messages using [`miette`].

```text
  × expected identifier
   ╭─[1:1]
 1 │
 2 │ pub struct {
   ·           ┬
   ·           ╰── expected identifier
 3 │     num_yaks: usize
   ╰────
```

# Usage
```rust
let source = r"
pub struct {
    num_yaks: usize
}";

let error = syn::parse_str::<syn::DeriveInput>(source).unwrap_err();
let error = syn_miette::Error::new(error, source);

let rendered = render(error); // See miette documentation for usage

println!("{}", rendered);

```


Notably, [`Error`] properly renders children that have been [`syn::Error::combine`]-ed:
```text
 × duplicate definition of `Foo`
  ╭─[1:1]
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
