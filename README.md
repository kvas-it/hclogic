# Basic constructive logic prover

I tried to program a prover that would be able to do exercises from the book "Type Theory & Functional Programming" by Simon Thompson. So far it can prove some simple formulas:

    $ runhaskell Main.hs

	proving A => A v B
	>>> (\a : A). inl a
	
	proving A => B => A ^ B
	>>> (\a : A). (\b : B). (a, b)
	
	proving A ^ B => A v B
	>>> (\a : A ^ B). inl (fst a)
	
	proving (A => B) ^ (B => C) => A => C
	>>> (\a : (A => B) ^ (B => C)). (\b : A). snd a (fst a b)
	
	proving A ^ (B ^ C) => (A ^ B) ^ C
	>>> (\a : A ^ (B ^ C)). ((fst a, fst (snd a)), snd (snd a))
	
	proving (A v B) ^ (A => C) ^ (B => C) => C
	>>> (\a : (A v B) ^ (A => C) ^ (B => C)). cases (fst a) (fst (snd a)) (snd (snd a))
	
	proving (A => C) ^ (B => C) => A v B => C
	>>> (\a : (A => C) ^ (B => C)). (\b : A v B). cases b (fst a) (snd a)
	
	proving (A ^ B => C) ^ A => B => C
	failed

The code is a bit messy and incomplete (for example can't prove the last formula and doesn't know how to deal with negation) but it's been fun.

Note: the notation (inl, inr, etc.) has been taken from the book:

* `inl` produces a proof of `A v B` from a proof of `A`.
* `inr` produces a proof of `A v B` from a proof of `B`.
* `fst` produces a proof of `A` from a proof of `A ^ B` (second proof is a pair of proofs for `A` and `B`, so `fst` just takes the first element of the pair).
* `snd` produces a proof of `B` from a proof of `A ^ B`.
* `cases` is a function of 3 arguments that expects `inl x` or `inr x` as its first argument. It's used for eliminating disjunction and is defined as follows:

		cases (inl x) f g = f x
		cases (inr x) f g = g x

* Finally the lambda form `(\x : A). p` is a function that takes a proof of `A` and substitutes it for `x` in `p`.