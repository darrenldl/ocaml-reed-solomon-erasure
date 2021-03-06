# ocaml-reed-solomon-erasure

## This project has been migrated to GitLab
You can view the new project repo [here](https://gitlab.com/darrenldl/ocaml-reed-solomon-erasure)

[Documentation](https://darrenldl.github.io/ocaml-reed-solomon-erasure/)

OCaml implementation of Reed-Solomon erasure coding

This is a port of [reed-solomon-erasure](https://github.com/darrenldl/reed-solomon-erasure), which is a port of several other libraries

#### Port progress
- [x] `galois.rs` -> `galois.ml` + `galois.mli`
- [x] `matrix.rs` -> `matrix.ml` + `matrix.mli`
- [x] `errors.rs` -> `errorrs.ml` + `errors.mli`
- [x] `inversion_tree.rs` -> `inversion_tree.ml `+ `inversion_tree.mli`
- [ ] `misc_utils.rs` -> `misc_utils.ml` + `misc_utils.mli`
- [ ] `shard_utils.rs` -> `shard_utils.ml` + `shard_utils.mli`
- [ ] `lib.rs` -> `reed_solomon_erasure.ml` + `reed_solomon_erasure.mli`
- [x] `galois_tests.rs` -> `galois_tests.ml`
- [x] `matrix_tests.rs` -> `matrix_tests.ml`
- [x] `inversion_tree_tests.rs` -> `inversion_tree_tests.ml`
- [ ] `errors_tests.rs` -> `errors_tests.ml`
- [ ] `lib_tests.rs` -> `lib_tests.ml`

## Usage
TODO

## Example
TODO

## Performance

## Changelog
[Changelog](CHANGELOG.md)

## Contributions
Contributions are welcome. Note that by submitting contributions, you agree to license your work under the same license used by this project(MIT).

## Notes
#### Code quality review
If you'd like to evaluate the quality of this library, you may find audit comments helpful.

Simply search for "AUDIT" to see the dev notes that are aimed at facilitating code reviews.

## License
All files are released under the MIT License
