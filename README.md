# malli-forms
Generate HTML forms using your [malli](https://github.com/metosin/malli) schemas.

## Goals
### Primary
1. Simple form generation from schema.
2. Easy to override behavior on a case-by-case basis.
3. Pre-fill fields with a partial object.
4. Small helper functions to address common use cases:
   - decoder with transformers to remove extra keys etc; should play nicely with common ring parsing.
   - short-circuit on validation errors; re-render with errors from `m/explain`.
5. Require few additions to existing schemas to work.

### Secondary
1. Easy overall theming.
2. Optional parser for actual form string.
3. Autogenerate [reitit](https://github.com/metosin/reitit) endpoints with appropriate coercion based on a given schema.
5. #5, but based on schema in function metadata.

## Rationale
Writing forms by hand is no fun. Even with a terse templating language
like hiccup, a simple form that POSTs to the same URL is guaranteed to
contain a lot of boilerplate:
```clojure
(require '[ring.util.anti-forgery :refer [anti-forgery-field]])
[:form {:method "POST"}
 (anti-forgery-field)
 [:label {:for "user-email"} "User Email"]
 [:input#user-email {:name "user-email" :type "email" :required true}]
 [:input {:type "submit" :value "Reset user password"}]]
```
Three places to keep track of `user-email`, and state is embedded in
the markup with the anti-forgery field - this essentially needs to be
embedded in a function, complecting the structure of the markup with the
information we care about.

### One solution: [Formative](https://github.com/jkk/formative)
The same form as above (roughly) can be produced with Formative as follows:
```clojure
(formative.core/render-form
  {:fields [{:name :user-email
             :type :email}]
   :validations [:required [:user-email]]})
```
With the default `bootstrap-horizontal` renderer, it even generates a
label, `User email`. This is a lot better.

#### The good
##### Writing & rendering
1. Very low-boilerplate - just the essentials, not the markup
2. Control over field order
3. Defaults to `POST`
4. Automatic embedding of `anti-forgery-token` on `POST` forms
5. Automatic label generation
6. Decent validation spec
7. Reasonably easy to write a custom renderer
##### Parsing & validation
1. Removes undefined keys - no unexpected input
2. Enforces validations server-side
   1. Macro for handling failures is nice
   2. Error data structure is comprehensible
   3. Errors and values can be trivially added to form spec and re-rendered
3. Can parse raw form data

#### The not so good
##### Hard to add additional attributes to a field or form
Sure, you can add various expected keys to a form spec to have them
rendered as attributes, but only *expected* keys. This is also true of
fields. Outside of making it difficult to integrate forms with tools
like [Unpoly](https://github.com/unpoly/unpoly), the list of expected
attributes
[doesn't include `required`](https://github.com/jkk/formative/issues/63)[^1].

[^1]: This is also not an example of an attitude towards maintaining a
      library that inspires overwhelming confidence.

##### Doesn't always play nicely with common Ring parsing
Mostly to do with checkbox handling.

##### Duplication of effort
This is what it really comes down to, IMO: much of the information
involved in rendering a form is already present in a malli schema, if
you have one. Such a schema can also be used in multiple areas, as
opposed to a single-purpose form spec. 

## Development
```bash
clojure -A:test:dev -M:repl/rebel
```
