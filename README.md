## Overview

This library studies two concepts: <algormithm> rewritten for
iterator-terminator pairs, and iterator adaptors as a way to customize and
implement new algorithms.

The signature for `copy`, as defined by the standard, is such:
```c++
template<class InputIterator, class OutputIterator>
OutputIterator copy(InputIterator first, InputIterator last,
                    OutputIterator result);
```
This works very well for the STL containers it was conceived for, however for
certain situations, `copy` does not work well. For example, one might want to
copy a certain number of objects that may be less than the container's size, or
perhaps copy all elements that meet a specific criteria. Therefor we have
`copy_n` and `copy_if`. But what about <algorithm> functions that do not have
an `_if` or `_n` variant? Should all functions supply one? What about a variant
like `copy_until`?

Those familiar with Eric Niebler's ranges proposal may suggest this is
unnecessary because we can use a filter or counted range.
```c++
template<class InputRange, class OutputIterator>
OutputIterator copy(InputRange rng, OutputIterator result);

// psuedo-code
copy(rng | filter(pred), out);      // copy_if:
copy(rng | take(n), out)            // copy_n
copy(rng | take_until(pred), out);  // copy_until
```
This very elegantly solves the problem for ranges, but code using iterators
remains in the dark. While the ranges proposal does recommend changes to
<algorithm> such that iterator versions of the above should be possible, the
[implementation](https://github.com/ericniebler/range-v3) does not offer such
utilities.

This library implements both <algorithm> and iterator adaptors to enable this
functionality without the use of ranges by allowing the terminator to have a
different type than the iterator for the range.
```c++
template<class InputIterator, class Terminator, class OutputIterator>
OutputIterator copy(InputIterator first, Terminator last,
                    OutputIterator result);

copy(filter(first, last, pred), seq_end{}, out);  // copy_if
copy(counter(first), take(n), out);               // copy_n
copy(first, predicate(pred), out);                // copy_until
```
While the above code may not look as pretty as the range-based version, it
serves as a case-in-point that we have not exhausted the possibilities of
iterators themselves, and that allowing `<algorithm>`'s functions to accept
iterator-terminator pairs can make them much more extensible.

# Is this an STL or ranges replacement?

This library is not intended for any real use. Its functions are not as
optimized as gcc, clang, or MSVC's implementations, some are missing, and some
may have the wrong complexity. I would not recommend the use of this over Mr
Niebler's range-v3 library. It is for demonstration purposes only.

# Iterators and Terminators

## iter_ref()
**Iterator category:** `std::iterator_traits<Iterator>::iterator_category`

**reference:** `std::iterator_traits<Iterator>::reference`

`iter_ref(iter)` is similar to if you wrote `std::ref(iter)`, except that
`std::reference_wrapper`s can't be incremented, dereferenced, or passed to
<algorithm> functions.

## counting_iterator<Iterator> and take
**Iterator category:** `std::iterator_traits<Iterator>::iterator_category`

**Reference:** `std::iterator_traits<Iterator>::reference`

The function `counter(iter, n=0)` constructs a counting iterator, `c`, which
contains a base iterator (`c.base()`) and a count (`c.count`).
```c++
// Will terminate when first == last || first.count == n.
copy(counter(first), counter(last, n), out);
```
This is nice if you don't know whether `last` or `first + n` will come first,
but you might not know where `last` is, so `take` can be used.
```c++
copy(counter(first), take(n), out);  // copy_n
```
`take` is a struct with just one member, `count`, for comparison against
counting iterators.

## sentinel_iterator<Value> and predicate_iterator<Predicate>
**Iterator category:** N/A

**Reference:** N/A

`sentinel(val)` returns an iterator that terminates a range based on its value.

`predicate(pred)` returns an iterators that terminates on `pred`.
```c++
// Prints "a string"
for_each("a string", sentinel(0), [](char c){ std::cout << c; });

// Takes two passes, without predicate_iterator.
auto it = find_if(first, last, pred);
transform(first, it, f);

// Only one pass, but may overrun the container.
transform(first, predicate(pred), f);

// One pass and safe:
transform(first, either(predicate(pred), last), f);
```

## generator_iterator<Function>
**Iterator category:** `std::input_iterator_tag`

**Reference:** `std::result_of_t<Function()> &`

`generator(f)` returns an iterator, `g`, with a value, `g.value` it updates on
every increment with `g.value = g.f()`. Consider this example using the
Fibonacci sequence:
```c++
struct Fib {
  int last = 0;
  int current = 1;

  int operator() () {
    int next = last + current;
    last = current;
    current = next;
    return current;
  }
};

// The sum of all Fibonacci numbers below 100:
int sum = accumulate(generator(Fib{}),
                     predicate([](int val) { return val < 100; }));
```

## transform_iterator<Iterator, UnaryOperation>
**Iterator category:** `std::iterator_traits<Iterator>::iterator_category`

**Reference:** `std::result_of_t<UnaryOperation(Iterator::reference)>`

`trans_iter(iter, f)` returns an iterator adaptor, `t`, such that `*t = f(*iter)`.
```c++
template<class InputIterator, class Terminator,
         class OutputIterator,
         class UnaryOperation>
OutputIterator
transform(InputIterator first, Terminator last,
          OutputIterator result, UnaryOperation op)
{
  return copy(trans_iter(first, std::move(op)), last, result);
}
``` 

## filter_iterator<InputIterator, Terminator, Predicate>
**Iterator category:** `std::input_iterator_tag`

**Reference:** `std::iterator_traits<InputIterator>::reference`

Normally, in order to run an algorithm off of a filtered range, one would need
to `copy_if` into a new containers first, making it a two-pass algorithm and
involve unnecessary copying. Even if one constructs the second container with
references to the first, the cost of making the second container is
unnecessary. Thus, the user will likely make a hand-written loop instead.

`filter(first, last, pred)` constructs a `filter_iterator`, `f`, allowing the
above situation to avoid making two passes and copying. If no element in the
range `[first, last)` satisfies `pred`, then `f == last`. Alternately, since
`f` contains its complete range, one could equivalently write `f == seq_end{}`.

```c++
template<class ForwardIterator, class Terminator, class Predicate>
ForwardIterator remove_if(ForwardIterator first, Terminator last,
                          Predicate pred)
{
  first = find_if(first, last, pred);
  if (first == last)
    return first;

  // remove_if is simply a move of every element that should be kept into the
  // range that starts with the first element to remove.
  return move(filter(first, last, not_fn(pred)), seq_end(), first);
}

template<class InputIterator, class Terminator, class Predicate>
typename std::iterator_traits<InputIterator>::difference_type
count_if(InputIterator first, Terminator last, Predicate pred)
{
  using Count = typename std::iterator_traits<InputIterator>::difference_type;
  Count c = 0;

  for_each(filter(std::move(first), last, std::move(pred)),
           last,
           [&](const auto&) { c++; });

  return c;
}
```

## joined_iterators<Iterator1, Iterator2> and joined_first_end<Iterator>

**Iterator category:** The std::common_type_t of Iterator1 and 2's iterator
category.

Many algorithms require iterating through two sequences, but most have only
been designed for one. Again, the user will likely resort to a hand-written
loop.

`iter_join(iter1, iter2)` returns a joined iterator, `j`, such that `j++`
increments both iterators and `*j` returns a pair,
`std::make_pair(std::ref(*iter1), std::ref(*iter2))`. To adapt a binary
function meant for calling with `bin(*iter1, *iter2)`, one can use the result
of `unpack_pair(bin)`. Many algorithms only require a terminator on the first
iterator, so `fst_end(term1)` can be used to express the end.
```c++
// Short and simple implementation of the binary overload of std::transform.
template<class InputIterator1, class Terminator1,
         class InputIterator2,
         class OutputIterator, class BinaryOperation>
OutputIterator
transform(InputIterator1 first1, Terminator1 last1,
          InputIterator2 first2, OutputIterator result,
          BinaryOperation binary_op)
{
  return transform(iter_join(first1, first2), fst_end(last1), result,
                   unpack_pair(binary_op));
}

// Implementation of std::equal:
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class BinaryPredicate>
bool equal(InputIterator1 first1, Terminator1 last1,
           InputIterator2 first2, Terminator2 last2,
           BinaryPredicate pred)
{
  return all_of(iter_join(iter_ref(first1), iter_ref(first2)),
                iter_join(last1, last2),
                unpack_pair(std::move(pred)))
    && first1 == last1 && first2 == last2;
}
```
`iter_prod(iter1, iter2, bin_op)` returns a joined iterator, `p`, but such that
`*p = bin_op(*iter1, *iter2)`.
```c++
template <class InputIterator1, class Terminator,
          class InputIterator2,
          class T,
          class BinaryOperation1 = std::plus<>,
          class BinaryOperation2 = std::multiplies<>>
T inner_product(InputIterator1 first1, Terminator last1,
                InputIterator2 first2, T init,
                BinaryOperation1 binary_op1 = BinaryOperation1(),
                BinaryOperation2 binary_op2 = BinaryOperation2())
{
  return  accumulate(iter_prod(first1, first2, binary_op2),
                     fst_end(last1),
                     init,
                     binary_op1);
}
```


