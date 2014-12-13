Data.Kiln
=========

We functional programmers know that mutable state is unglamorous, but sometimes useful. `Data.Kiln` lets you create and manipulate mutable recursive structures (`Clay`) in the `Squishy` monad (built over `ST`), then once you're finished with mutation, bake them into immutable, pure, lazy data. The library is polymorphic in the "shape" of each mutable "node" in the graph, requiring only that it be Traversable, which means that all sorts of structures, from cons-cells to tagged state diagrams, can be represented and manipulated safely and efficiently, with an ultimately pure output.
