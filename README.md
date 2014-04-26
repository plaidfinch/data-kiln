Midas
=====

We functional programmers know that mutable state is unglamorous, but sometimes useful (much like lead) and pure, immutable data structures are desirable and beautiful (like gold). The Midas library lets you create and manipulate mutable graph structures in the ST monad, then turn them to gold -- i.e. immutable, pure, cyclic lazy data structures. The library is polymorphic in the "shape" of each mutable "node" in the graph, requiring only that it be Traversable, which means that all sorts of structures, from cons-cells to tagged state diagrams, can be represented and manipulated safely and efficiently, with an ultimately pure output. Chrysopoeia in Haskell!
