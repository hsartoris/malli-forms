# Notes on walking in malli
all of the functions accept options as final arg

## `m/Schema`
### `m/-walk`
	- called on a schema object primarily, with Walker provided
	- returns schema as transformed by Walker, but that can happen many ways
	- allows schema to define how it should be walked, including such issues
	as child schemas, which are otherwise opaque to the walker
	- most (all?) core schemas call m/-accept to check that Walker wants to
	work with the schema; the purpose of this appears to be early
	termination, as evidenced by its usage in malli.util
	- typical call structure looks like:
	```clojure
(when (m/-accept walker this path options)
 (m/-outer walker this path
	(map #(m/-inner walker % (conj path "path relevant for schema"))
	 children)
	options))
	```

## `m/Walker`
### `m/-accept`
	- as above, allows early termination

### `m/-inner`
	- Walker is provided a schema, implicitly a child of schema currently
	being walked, and path
	- This should return the child schema as transformed by Walker - overlap
	with m/-walk is intentional; default m/walk Walker simply calls m/-walk
	from m/-inner
	- Essentially an 'enter' phase of walking: allows advanced behavior to be
	executed *before* traversing a child schema, e.g. modifying the path

### `m/-outer`
	- Walker is provided a schema, a path, and walked children
	- This is where the actual transform of the schema is largely expected to
	take place: m/walk implements this as just calling the provided function
	on the four args.
	- Must set the walked children to be the children of the schema -
	returning the schema without doing so will return the original, unwalked
	children.


