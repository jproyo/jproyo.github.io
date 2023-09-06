+++
title = "Building Diesel Macro for generating FromSql and ToSql derivation on Enum Postgresql types."
date = 2023-09-06

[taxonomies]
tags = ["rust", "macros", "proc-macro", "diesel", "orm", "tutorial"]

+++

# Building Diesel Macro for generating `FromSql` and `ToSql` derivation on Enum PostgreSQL types: A Code-First Guide
In this blog post, we will explore how to build a simple macro `crate` to automatic generate derivations for `ToSql` and `FromSql` in the case of PostgreSQL binding for `diesel`. This simplifies handling enums in **Rust** when working with **Diesel** and **PostgreSQL**. Instead of focusing on publishing the crate, we will provide a step-by-step guide on how to create the procedural macro and use it in your Rust projects.

The crate is already published by me here [https://crates.io/crates/diesel_sqltype_enum_pg](https://crates.io/crates/diesel_sqltype_enum_pg).

## Step 1: Create a New Rust Crate
Begin by creating a new Rust crate for our procedural macro. Use Cargo, Rust's package manager, to initialize a new project with the following command:

```shell
cargo new enum_diesel_macros
```

This command generates a new directory named `enum_diesel_macros` with the required project structure.

## Step 2: Define the Procedural Macro

In the `enum_diesel_macros` crate, the most crucial step is defining the procedural macro. Below is the code for the procedural macro:


```rust
#[proc_macro_derive(FromToSql, attributes(fromtosql))]
pub fn describe(input: TokenStream) -> TokenStream {
    // Parse the input and extract relevant information
    let DeriveInput {
        data, ident, attrs, ..
    } = parse_macro_input!(input);

    // Ensure that the macro is applied to an enum
    match data {
        syn::Data::Enum(DataEnum { .. }) => {}
        _ => {
            panic!("Only supported for enum type")
        }
    };

    // Extract attributes provided in the derive macro
    let binding = attrs
        .iter()
        .filter(|a| a.path().is_ident("fromtosql"))
        .flat_map(|a| {
            let mut p = Vec::new();
            let parser = a
                .parse_args_with(Punctuated::<syn::Ident, Eq>::parse_separated_nonempty)
                .unwrap();
            let ident = parser.first().cloned().map(|f| f.to_string());
            let value = parser.last().cloned();
            if let Some("sql_type") = ident.as_deref() {
                if let Some(value) = value {
                    p.push(value)
                }
            }
            p
        })
        .collect::<Vec<_>>();

    let att = match binding.first() {
        Some(idnt) => idnt,
        None => panic!("`companion` attribute not found"),
    };

    // Generate code for ToSql and FromSql implementations
    let output = quote! {
         impl ::diesel::serialize::ToSql<#att, ::diesel::pg::Pg> for #ident {
             fn to_sql<'b>(&'b self, out: &mut ::diesel::serialize::Output<'b, '_, ::diesel::pg::Pg>) -> ::diesel::serialize::Result {
                 use ::std::io::Write;
                 out.write_all(self.to_string().as_bytes())?;
                 Ok(::diesel::serialize::IsNull::No)
             }
         }

         impl FromSql<#att, ::diesel::pg::Pg> for #ident {
            fn from_sql(bytes: ::diesel::pg::PgValue) -> ::diesel::deserialize::Result<Self> {
                use ::std::str::FromStr;
                let value: String = <String as FromSql<::diesel::sql_types::Text, ::diesel::pg::Pg>>::from_sql(bytes)?;
                #ident::from_str(value.as_str())
                    .map_err(|e| format!("Error converting from PgValue {:?}", e).into())
            }
        }
    };

    // Return the generated code as a TokenStream
    output.into()
}
```

This procedural macro handles the generation of `FromSql` and `ToSql` implementations for your enums, making enum handling in Diesel more straightforward.

Note a couple of things in this code.

1. We are pattern matching in the **AST** for allowing only configure this macro on `Enum` types.
```rust
    // Ensure that the macro is applied to an enum
    match data {
        syn::Data::Enum(DataEnum { .. }) => {}
        _ => {
            panic!("Only supported for enum type")
        }
    };


```
2. Then we iterate
Here we over the attributes inside the macro, to be sure that the companion `sql_type` is set in order to enable the compiler to verify that declaration inside Diesel schema.

```rust
 // Extract attributes provided in the derive macro
    let binding = attrs
        .iter()
        .filter(|a| a.path().is_ident("fromtosql"))
        .flat_map(|a| {
            let mut p = Vec::new();
            let parser = a
                .parse_args_with(Punctuated::<syn::Ident, Eq>::parse_separated_nonempty)
                .unwrap();
            let ident = parser.first().cloned().map(|f| f.to_string());
            let value = parser.last().cloned();
            if let Some("sql_type") = ident.as_deref() {
                if let Some(value) = value {
                    p.push(value)
                }
            }
            p
        })
        .collect::<Vec<_>>();

    let att = match binding.first() {
        Some(idnt) => idnt,
        None => panic!("`companion` attribute not found"),
    };
```

3. Finally, we are able to generate the derivations and add it to the `TokenStream` of the **AST**.

```rust
    // Generate code for ToSql and FromSql implementations
    let output = quote! {
         impl ::diesel::serialize::ToSql<#att, ::diesel::pg::Pg> for #ident {
             fn to_sql<'b>(&'b self, out: &mut ::diesel::serialize::Output<'b, '_, ::diesel::pg::Pg>) -> ::diesel::serialize::Result {
                 use ::std::io::Write;
                 out.write_all(self.to_string().as_bytes())?;
                 Ok(::diesel::serialize::IsNull::No)
             }
         }

         impl FromSql<#att, ::diesel::pg::Pg> for #ident {
            fn from_sql(bytes: ::diesel::pg::PgValue) -> ::diesel::deserialize::Result<Self> {
                use ::std::str::FromStr;
                let value: String = <String as FromSql<::diesel::sql_types::Text, ::diesel::pg::Pg>>::from_sql(bytes)?;
                #ident::from_str(value.as_str())
                    .map_err(|e| format!("Error converting from PgValue {:?}", e).into())
            }
        }
    };

    // Return the generated code as a TokenStream
    output.into()
```

Another important thing to notice is that this relies in `ToString` and `FromStr` traits on the defined Enum allowing the user of the macro, to change the name of the enum according to the name of the enum defined in the Database.

## Step 3: Configure Your Cargo.toml
To make your procedural macro crate usable by others, you need to configure your Cargo.toml file. Add the following dependencies:

```toml
[dependencies]
syn = "1.0"
quote = "1.0"
diesel = "x.x.x"
```

Replace **"x.x.x"** with the appropriate version of the Diesel crate. Additionally, specify the `proc-macro` feature:

```toml
[lib]
proc-macro = true
```

This configuration ensures that your crate can be used as a procedural macro.

## Step 4: Documentation and Examples
A good procedural macro crate should include documentation and examples to help users understand how to use it. Create a README.md file in your crate's root directory and provide usage instructions, examples, and any necessary information about your procedural macro. For more detail you should follow [Cargo Reference here](https://doc.rust-lang.org/cargo/reference/publishing.html)

## Conclusion

Congratulations! You've successfully built the `enum_diesel_macros` crate, a powerful tool for simplifying enum handling in Rust when using **Diesel and PostgreSQL**. This procedural macro streamlines the generation of `FromSql` and `ToSql` implementations for your enums, making your code cleaner and more efficient.

Now you have the knowledge to create procedural macros to enhance your Rust development experience with Diesel and PostgreSQL. Happy coding with Rust!
