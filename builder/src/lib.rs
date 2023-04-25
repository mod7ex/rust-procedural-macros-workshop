use core::panic;

use proc_macro2::{TokenTree, Delimiter, Punct, Literal};
use quote::{quote, ToTokens};
use proc_macro::TokenStream;
use syn::{
    parse_macro_input,
    Data,
    DataStruct,
    DeriveInput,
    Fields,
    FieldsNamed,
    Ident,
    spanned::Spanned,
    Type,
    PathArguments,
    GenericArgument, parse2
};

// https://youtu.be/geovSK3wMB8?t=7073
// https://www.youtube.com/watch?v=KVWHT1TAirU&ab_channel=JonGjengset

// https://users.rust-lang.org/t/syn-how-do-i-iterate-on-the-fields-of-a-struct/42600?u=modex98

fn is_optional(ty: &Type) -> bool {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            if segment.ident.to_string() == "Option" {
                return true;
            }
        }
    }
    false
}

fn generic<'a>(ty: &'a Type, target: &'a str) -> &'a Type {
    if let Type::Path(type_path) = ty {
        if let Some(segment) = type_path.path.segments.last() {
            let ident = &segment.ident;
            if ident.to_string() == target {
                if let PathArguments::AngleBracketed(args) = &segment.arguments {
                    if let Some(generic_arg) = args.args.first() {
                        if let GenericArgument::Type(inner_type) = generic_arg {
                            return inner_type;
                        }
                    }
                }
            }
        }
    }
    ty
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(tokens: TokenStream) -> TokenStream {
    let tokens = parse_macro_input!(tokens as DeriveInput);

    let entity_ident = tokens.ident;

    let entity_fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = tokens.data
    {
        named.iter().map(|f| {
            let mut spreadable = false;
            let mut method_ident = f.ident.clone().unwrap();
            for attr in &f.attrs {
                if let Some(TokenTree::Group(group)) = attr.to_token_stream().clone().into_iter().last() {
                    let mut tts = group.stream().into_iter();
                    let ts_ident = tts.next().unwrap().to_token_stream();
                    let ident = parse2::<Ident>(ts_ident).unwrap();
                    if ident.to_string() == "builder"  {
                        if let TokenTree::Group(group) = tts.next().unwrap() {
                            if group.delimiter() != Delimiter::Parenthesis {
                                panic!("Wrong usage use parenthesis");
                            }

                            let mut stream_iter = group.stream().into_iter();

                            let inert_ident = parse2::<Ident>(stream_iter.next().unwrap().to_token_stream()).unwrap();
                            if inert_ident.to_string() != "each" {
                                panic!("Wrong usage inert ident should be \"each\"");
                            }

                            let punct = parse2::<Punct>(stream_iter.next().unwrap().to_token_stream()).unwrap();
                            if punct.as_char() != '=' {
                                panic!("Wrong usage punct should be '='");
                            }

                            let literal = parse2::<Literal>(stream_iter.next().unwrap().to_token_stream()).unwrap();
                            let literal_str = &literal.to_string().replace("\"", "");
                            /*
                                let expected_literal = &f.ident.to_token_stream().to_string();
                                if &format!("{}s", literal_str) != expected_literal && literal_str != expected_literal {
                                    panic!("Wrong usage wrong field name in attr");
                                }
                            */
                            method_ident = Ident::new(literal_str, f.span().clone());
                            spreadable = true;
                        } else {
                            panic!("Wrong usage");
                        }
                    }
                }
            }

            (f.ident.clone().unwrap(), generic(&f.ty, "Option"), is_optional(&f.ty), spreadable, method_ident)
        })
    } else {
        panic!("Invalid input");
    };

    let mut builder_fields = quote!();
    let mut raw_builder_instance = quote!();
    let mut entity_builder_implementation = quote!();
    let mut build_body = quote!();

    for (field_ident, ty, is_optional, spreadable, method_ident) in entity_fields {

        builder_fields.extend(quote! {
            #field_ident: Option<#ty>,
        });

        raw_builder_instance.extend(quote! {
            #field_ident: None,
        });

        entity_builder_implementation.extend({
            if spreadable {
                let m_ty = generic(ty, "Vec");
                quote! {
                    pub fn #method_ident(&mut self, v: #m_ty) -> &mut Self {
                        if let Some(prev) = self.#field_ident.as_mut() {
                            prev.push(v);
                        } else {
                            self.#field_ident = Some(vec![v])
                        }
                        self
                    }
                }
            } else {
                quote! {
                    pub fn #field_ident(&mut self, v: #ty) -> &mut Self {
                        self.#field_ident = Some(v);
                        self
                    }
                }
            }
        });

        let err_msg = format!("field <{}> isn't set", field_ident);
        build_body.extend({
            if is_optional {
                quote! {
                    #field_ident: self.#field_ident.clone(),
                }
            } else {
                quote! {
                    #field_ident: self.#field_ident.clone().expect(#err_msg),
                }
            }
        });
    }

    let ident_str = format!("{}Builder", entity_ident);

    let entity_builder_ident = Ident::new(&ident_str, ident_str.span());

    quote! {
        impl #entity_ident {
            pub fn builder() -> #entity_builder_ident {
                #entity_builder_ident {
                    #raw_builder_instance
                }
            }
        }

        // #[derive(Debug)]
        pub struct #entity_builder_ident {
            #builder_fields
        }

        impl #entity_builder_ident {
            #entity_builder_implementation

            pub fn build(&self) -> Result<#entity_ident, Box<dyn ::std::error::Error>> {
                Ok(
                    #entity_ident {
                        #build_body
                    }
                )
            }
        }
    }.into()
}

/* 

#[proc_macro_derive(Builder)]
pub fn derive(tokens: TokenStream) -> TokenStream {
    let tokens = parse_macro_input!(tokens as DeriveInput);

    let entity_ident = tokens.ident;

    let entity_fields = if let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { ref named, .. }),
        ..
    }) = tokens.data
    {
        named.iter().map(|f| (&f.ident, generic(&f.ty), is_optional(&f.ty)))
    } else {
        panic!("Invalid input");
    };

    let mut builder_fields = vec![];
    let mut raw_builder_instance = vec![];
    let mut entity_builder_implementation = vec![];
    let mut build_body = vec![];

    for (ident, ty, is_optional) in entity_fields {
        let field_ident = &ident.clone().unwrap();

        // println!("{} {} {}", field_ident, is_optional, ty.to_token_stream().to_string());

        builder_fields.push(quote! {
            #field_ident: Option<#ty>
        });

        raw_builder_instance.push(quote! {
            #field_ident: None
        });

        entity_builder_implementation.push(quote! {
            pub fn #field_ident(&mut self, v: #ty) -> &mut Self {
                self.#field_ident = Some(v);
                self
            }
        });

        let err_msg = format!("field <{}> isn't set", field_ident);
        build_body.push({
            if is_optional {
                quote! {
                    #field_ident: self.#field_ident.clone()
                }
            } else {
                quote! {
                    #field_ident: self.#field_ident.clone().expect(#err_msg)
                }
            }
        });
    }

    let ident_str = format!("{}Builder", entity_ident);

    let entity_builder_ident = Ident::new(&ident_str, ident_str.span());

    quote! {
        impl #entity_ident {
            pub fn builder() -> #entity_builder_ident {
                #entity_builder_ident {
                    #(#raw_builder_instance,)*
                }
            }
        }

        pub struct #entity_builder_ident {
            #(#builder_fields,)*
        }
        
        impl #entity_builder_ident {
            #(#entity_builder_implementation)*

            pub fn build(&self) -> Result<#entity_ident, Box<dyn ::std::error::Error>> {
                Ok(
                    #entity_ident {
                        #(#build_body,)*
                    }
                )
            }
        }
    }.into()
}

*/