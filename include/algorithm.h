
#pragma once

#include <functional>
#include <iterator>
#include <type_traits>
#include <utility>

#include "iterators.h"
#include "concepts.h"

namespace alt {

template<class ForwardIterator,
         class T, class Compare = std::less<>>
ForwardIterator
_lower_bound(ForwardIterator first, ForwardIterator last,
            const T& value, Compare comp = Compare())
{
  ForwardIterator mid = first;
  while (distance(first, last) > 1) {
    mid = mid_point(first, last);
    if (comp(*mid, value))
      first = mid;
    else
      last = mid;
  }
  return last;
}

template<class ForwardIterator, class T, class Compare = std::less<>>
ForwardIterator
_upper_bound(ForwardIterator first, ForwardIterator last,
            const T& value, Compare comp = Compare())
{
  return lower_bound(first, last, value, flip(not_fn(comp)));
}

/// 25.2 -- Non-modifying sequence operations

/// 25.2.1 -- All of
template<class InputIterator, class Terminator, class Predicate>
bool all_of(InputIterator first, Terminator last, Predicate pred) {
  require_input_range<InputIterator, Terminator>();
  require_is_predicate<Predicate, decltype(*first)>();

  bool ret = true;
  for(; first != last && ret; first++)
    ret = pred(*first);
  return ret;
}

/// 25.2.2 -- Any of
template <class InputIterator, class Terminator, class Predicate>
bool any_of(InputIterator first, Terminator last, Predicate pred) {
  return filter(first, last, std::move(pred)) != last;
}

/// 25.2.3 None of
template <class InputIterator, class Terminator, class Predicate>
bool none_of(InputIterator first, Terminator last, Predicate pred) {
  return filter(first, last, std::move(pred)) == last;
}

/// 25.2.4 For each
template<class InputIterator, class Terminator, class Function>
Function for_each(InputIterator first, Terminator last, Function f) {
  require_input_range<InputIterator, Terminator>();

  for(; first != last; first++)
    f(*first);

  return std::move(f);
}

/// 25.2.5 Find
template<class InputIterator, class Terminator, class Predicate>
InputIterator find_if_not(InputIterator first, Terminator last, Predicate pred)
{
  return filter(std::move(first),
                std::move(last),
                [&](auto&& x){ return !pred(x); }).base();
}

template<class InputIterator, class Terminator, class Predicate>
InputIterator find_if(InputIterator first, Terminator last,
                      Predicate pred)
{
  return find_if_not(first, last, [&](auto&& x) { return !pred(x); });
}

template<class InputIterator, class Terminator, class T>
InputIterator find(InputIterator first, Terminator last,
                   const T& value)
{
  require_equality_comparible<decltype(*first), const T&>();

  return find_if(first, last, [&](auto&& x) { return x == value; });
}

/// 25.2.9 Count [alg.count]
template<class InputIterator, class Terminator, class Predicate>
typename std::iterator_traits<InputIterator>::difference_type
count_if(InputIterator first, Terminator last, Predicate pred)
{
  require_input_range<InputIterator, Terminator>();

  using Count = typename std::iterator_traits<InputIterator>::difference_type;
  Count c = 0;

  for_each(filter(std::move(first), last, std::move(pred)),
           last,
           [&](const auto&) { c++; });

  return c;
}

template<class InputIterator, class Last, class T>
typename std::iterator_traits<InputIterator>::difference_type
count(InputIterator first, Last last, const T& value)
{
  require_equality_comparible<decltype(*first), const T&>();

  return count_if(first, last, [&](auto& x) { return x == value; });
}

/// 25.2.11 Equal [alg.equal]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class BinaryPredicate>
bool equal(InputIterator1 first1, Terminator1 last1,
           InputIterator2 first2, Terminator2 last2,
           BinaryPredicate pred)
{
  require_input_range<InputIterator1, Terminator1>();
  require_input_range<InputIterator2, Terminator2>();
  require_is_predicate<BinaryPredicate, decltype(*first1), decltype(*first2)>();

  return all_of(iter_join(iter_ref(first1), iter_ref(first2)),
                iter_join(last1, last2),
                unpack_pair(std::move(pred)))
    && first1 == last1 && first2 == last2;
}

template<class InputIterator1, class Terminator,
         class InputIterator2,
         class BinaryPredicate>
auto equal(InputIterator1 first1, Terminator last1,
           InputIterator2 first2, BinaryPredicate pred)
  -> decltype(pred(*first1, *first2), true)
{
  require_input_range<InputIterator1, Terminator>();
  require_input_iterator<InputIterator2>();
  require_is_predicate<BinaryPredicate, decltype(*first1), decltype(*first2)>();

  return all_of(iter_join(iter_ref(first1), iter_ref(first2)),
                fst_end(last1),
                unpack_pair(std::move(pred)));
}

template<class InputIterator1, class EqualityComparible1, class InputIterator2>
bool equal(InputIterator1 first1, EqualityComparible1 last1,
           InputIterator2 first2)
{
  require_equality_comparible<decltype(*first1), decltype(*first2)>();
  return equal(first1, last1, first2, std::equal_to<>());
}

template<class InputIterator1, class EqualityComparible1,
         class InputIterator2, class EqualityComparible2>
auto equal(InputIterator1 first1, EqualityComparible1 last1,
           InputIterator2 first2, EqualityComparible2 last2)
  -> decltype(first1 == last1 && first2 == last2, true)
{
  return equal(first1, last1, first2, last2, std::equal_to<>());
}


/// 25.3 Mutating sequence operations [alg.modifying.operations]

/// 25.3.1 Copy [alg.copy]
template<class InputIterator, class Terminator,
         class OutputIterator>
OutputIterator copy(InputIterator first, Terminator last,
                    OutputIterator result)
{
  require_io_range<InputIterator, Terminator, OutputIterator>();

  for (; first != last; first++, result++)
    *result = *first;
  return result;
}

template<class InputIterator, class Size, class OutputIterator>
OutputIterator copy_n(InputIterator first, Size n, OutputIterator result)
{
  return copy(counter(first), take(n), result);
}

template<class InputIterator, class Terminator, class OutputIterator, class Predicate>
OutputIterator copy_if(InputIterator first, Terminator last,
                       OutputIterator result,
                       Predicate pred)
{
  require_is_predicate<Predicate, decltype(*first)>();

  return copy(filter(first, last, std::move(pred)), seq_end(), result);
}

// TODO: copy_backwards

/// 25.3.2 Move [alg.move]
template<class InputIterator, class Terminator,
         class OutputIterator>
OutputIterator move(InputIterator first, Terminator last,
                    OutputIterator result)
{
  require_io_range<InputIterator, Terminator, OutputIterator>();
  for (; first != last; first++)
    *(result++) = std::move(*first);
  return result;
}

// TODO: move_backwards

/// 25.3.3 swap [alg.swap]
template<class ForwardIterator1, class Terminator, class ForwardIterator2>
ForwardIterator2
swap_ranges(ForwardIterator1 first1, Terminator last1,
            ForwardIterator2 first2)
{
  require_forward_range<ForwardIterator1, Terminator>();
  require_forward_iterator<ForwardIterator2>();

  for_each(iter_join(first1, iter_ref(first2)),
           fst_end(last1),
           [](auto&& pair) { std::swap(pair.first, pair.second); });
  return first2;
}

using std::iter_swap;

/// 25.3.4 Transform [alg.transform]
template<class InputIterator, class Terminator,
         class OutputIterator,
         class UnaryOperation>
OutputIterator
transform(InputIterator first, Terminator last,
          OutputIterator result, UnaryOperation op)
{
  return copy(trans_iter(first, std::move(op)), last, result);
}
  
template<class InputIterator1, class Terminator1,
         class InputIterator2,
         class OutputIterator, class BinaryOperation>
OutputIterator
transform(InputIterator1 first1, Terminator1 last1,
          InputIterator2 first2, OutputIterator result,
          BinaryOperation binary_op)
{
  require_input_range<InputIterator1, Terminator1>();
  require_input_iterator<InputIterator2>();

  using out = typename std::iterator_traits<OutputIterator>::reference;
  using bin_op_result = decltype(binary_op(*first1, *first2));
  static_assert(std::is_assignable<out, bin_op_result>::value,
                "the operation, `*result = binary_op(*first1, *first2)`, must be valid.");

  return transform(iter_join(first1, first2), fst_end(last1), result,
                   unpack_pair(binary_op));
}

/// 25.3.5 Replace [alg.replace]
template<class ForwardIterator, class Terminator, class Predicate, class T>
void replace_if(ForwardIterator first, Terminator last,
                Predicate pred, const T& new_value) {
  require_forward_range<ForwardIterator, Terminator>();
  copy_if(first, last,
          flipped(filler(std::cref(new_value))),
          pred);
}

template<class ForwardIterator, class Terminator, class T>
void replace(ForwardIterator first, Terminator last,
             const T& old_value, const T& new_value) {
  replace_if(first, last, [&](auto& x) { return x == old_value; }, new_value);
}

template<class InputIterator, class Terminator,
        class OutputIterator,
        class Predicate, class T>
OutputIterator
replace_copy_if(InputIterator first, Terminator last,
                OutputIterator result,
                Predicate pred, const T& new_value)
{
  return transform(first, last, result, 
                   [&](auto&& val) {
                     return pred(val) ? new_value : val;
                   });
}

template<class InputIterator, class Terminator,
        class OutputIterator, class T>
OutputIterator
replace_copy(InputIterator first, Terminator last,
             OutputIterator result,
             const T& old_value, const T& new_value)
{
  return replace_copy_if(first, last, result,
                         [&](auto& val) { return val == old_value; },
                         new_value);
}

/// 25.3.6 Fill [alg.fill]
template<class ForwardIterator, class Terminator, class T>
void fill(ForwardIterator first, Terminator last, const T& value)
{
  require_forward_range<ForwardIterator, Terminator>();
  copy(first, last, flipped(filler(std::cref(value))));
}

template<class OutputIterator, class Size, class T>
OutputIterator fill_n(OutputIterator first, Size n, const T& value)
{
  fill(counter(iter_ref(first)), take(n), value);
  return first;
}

/// 25.3.7 Generate [alg.generate]
template<class ForwardIterator, class Terminator, class Generator>
void generate(ForwardIterator first, Terminator last,
              Generator gen)
{
  require_forward_range<ForwardIterator, Terminator>();
  copy(first, last, flipped(generator(std::move(gen))));
};

template<class OutputIterator, class Size, class Generator>
OutputIterator generate_n(OutputIterator first, Size n, Generator gen)
{
  generate(counter(iter_ref(first)), take(n), std::move(gen));
  return first;
}

/// 25.3.8 Remove [alg.remove]
template<class ForwardIterator, class Terminator, class Predicate>
ForwardIterator remove_if(ForwardIterator first, Terminator last,
                          Predicate pred)
{
  require_forward_range<ForwardIterator, Terminator>();

  first = find_if(first, last, pred);
  if (first == last)
    return first;

  return move(filter(first, last, not_fn(pred)), seq_end(), first);
}

template<class ForwardIterator, class Terminator, class T>
ForwardIterator remove(ForwardIterator first, Terminator last,
                       const T& value)
{
  return remove_if(first, last, [&](auto&& val) { return val == value; });
}

template<class InputIterator, class Terminator,
         class OutputIterator, class Predicate>
OutputIterator
remove_copy_if(InputIterator first, Terminator last,
               OutputIterator result, Predicate pred)
{
  return copy_if(first, last, result, not_fn(pred));
}

template<class InputIterator, class Terminator,
         class OutputIterator, class T>
OutputIterator
remove_copy(InputIterator first, Terminator last,
            OutputIterator result, const T& value)
{
  return remove_copy_if(first, last, result,
                        [&](auto&& val) { return val != value; });
}

/// 25.3.9 Unique [alg.unique]
// Non-standard helper:
template<class ForwardIterator, class Terminator,
         class BinaryPredicate>
ForwardIterator adjacent_find(ForwardIterator first, Terminator last,
                              const BinaryPredicate& pred)
{
  if (first == last)
    return first;
  auto it = find_if(adjacent(first), last, unpack_pair(pred));
  return it.next == last ? it.next : it.base();
}

template<class InputIterator, class Terminator,
         class OutputIterator,
         class BinaryPredicate>
OutputIterator
unique_copy(InputIterator first, Terminator last,
            OutputIterator result, BinaryPredicate pred)
{
  require_input_range<InputIterator, Terminator>();
  require_is_predicate<BinaryPredicate, decltype(*result), decltype(*first)>();

  if (first == last)
    return result;

  for (; first != last; first++)
    if (!pred(*result, *first))
      *++result = *first;

  return ++result;
}

template<class InputIterator, class Terminator,
         class OutputIterator>
OutputIterator
unique_copy(InputIterator first, Terminator last,
            OutputIterator result)
{
  return unique_copy(first, last, result, std::equal_to<>());
}

template<class ForwardIterator, class Terminator,
         class BinaryPredicate>
ForwardIterator unique(ForwardIterator first, Terminator last,
                       BinaryPredicate pred)
{
  require_forward_range<ForwardIterator, Terminator>();
  first = adjacent_find(first, last, pred);
  if (first == last) return first;
  return unique_copy(std::next(first), last, first, pred);
}

template<class ForwardIterator, class Terminator>
ForwardIterator unique(ForwardIterator first, Terminator last)
{
  return unique(first, last, std::equal_to<>());
}

/// 25.3.10 Reverse [alg.reverse]
template<class BidirectionalIterator1, class BidirectionalIterator2,
         class OutputIterator>
OutputIterator
reverse_copy(BidirectionalIterator1 first, BidirectionalIterator2 last,
             OutputIterator result)
{
  require_bidirecitonal_range<BidirectionalIterator1, BidirectionalIterator2>();
  require_io_range<BidirectionalIterator1, OutputIterator>();

  return copy(make_reverse_iterator(last),
              first,
              result);
}

template<class BidirectionalIterator1, class BidirectionalIterator2>
void reverse(BidirectionalIterator1 first, BidirectionalIterator2 last)
{
  require_bidirecitonal_range<BidirectionalIterator1, BidirectionalIterator2>();
  
  while (first != last && first != --last)
    iter_swap(first++, last);
}

/// 25.3.11 Rotate [alg.rotate]
template<class ForwardIterator1, class ForwardIterator2, class Terminator>
ForwardIterator1 rotate(ForwardIterator1 first, ForwardIterator2 middle,
                       Terminator last)
{
  require_forward_range<ForwardIterator1, ForwardIterator2>();
  require_forward_range<ForwardIterator2, Terminator>();

  // TODO: This should return first + (last - middle). That means if
  // first == middle, perhaps last should be returned.
  if (first == middle || middle == last)
    return first;

  ForwardIterator2 next = middle;
  while (first != next) {
    iter_swap(first++, next++);
    if (next == last)         next = middle;
    else if (first == middle) middle = next;
  }

  return first;
}

template<class ForwardIterator1, class ForwardIterator2,
         class Terminator,
         class OutputIterator>
OutputIterator
rotate_copy(ForwardIterator1 first, ForwardIterator2 middle,
            Terminator last, OutputIterator result)
{
  return copy(first, middle, copy(middle, last, result));
}

/// 25.3.12 Random shuffle [alg.random.shuffle]
// TODO

/// 25.3.13 Partitions [alg.partitions]
template <class InputIterator, class Terminator, class Predicate>
bool is_partitioned(InputIterator first, Terminator last, Predicate pred)
{
  return none_of(find_if_not(first, last, pred), last, pred);
}

template<class ForwardIterator, class Terminator, class Predicate>
ForwardIterator
partition(ForwardIterator first, Terminator last, Predicate pred)
{
  require_forward_range<ForwardIterator, Terminator>();
  require_is_predicate<Predicate, decltype(*first)>();

  ForwardIterator middle = find_if_not(first, last, pred);

  first = middle;
  for (; middle != last; middle++)
    if (pred(*middle)) {
      iter_swap(first++, middle);
      while (first != middle && pred(*first))
        first++;
    }
  return first;
}

// TODO: stable_partition

template <class InputIterator, class Terminator,
          class OutputIterator1,
          class OutputIterator2, class Predicate>
std::pair<OutputIterator1, OutputIterator2>
partition_copy(InputIterator first, Terminator last,
               OutputIterator1 out_true, OutputIterator2 out_false,
               Predicate pred)
{
  for (; first != last; first++)
    if (pred(*first))
      *out_true++ = *first;
    else
      *out_false++ = *first;
  return std::make_pair(out_true, out_false);
}

template<class ForwardIterator, class Terminator, class Predicate>
ForwardIterator partition_point(ForwardIterator first,
                                Terminator last,
                                Predicate pred)
{
  // TODO: Can use binary search.
  return find_if_not(first, last, pred);
}

/// 25.4.1.1 sort [sort]
template<class RandomAccessIterator1, class RandomAccessIterator2,
         class Compare>
void sort(RandomAccessIterator1 first, RandomAccessIterator2 last,
          Compare comp)
{ 
  require_random_access_range<RandomAccessIterator1, RandomAccessIterator2>();
  if (last - first <= 1)
    return;

  auto split = partition(std::next(first), last,
                         [&](auto&& val) { return comp(val, *first); });
  iter_swap(first, split - 1);
  sort(first, split - 1, comp);
  sort(split, last, comp);
}

template<class RandomAccessIterator1, class RandomAccessIterator2>
void sort(RandomAccessIterator1 first, RandomAccessIterator2 last)
{
  return sort(first, last, std::less<>());
}

// TODO: stable_sort

/// 25.4.1.3 partial_sort [partial.sort]
template<class ForwardIterator, class Terminator, class Compare>
ForwardIterator _minimum(ForwardIterator first, Terminator last,
                         Compare comp)
{
  require_forward_range<ForwardIterator, Terminator>();
  require_is_predicate<Compare, decltype(*first), decltype(*first)>();

  if (first == last) return first;

  auto min_iter = first++;
  auto pred = [&](auto&& val) { return comp(val, *min_iter); };
  for (auto lows = filter(first, last, pred); lows; lows++)
    min_iter = lows.base();
  return min_iter;
}

template<class RandomAccessIterator1,
         class RandomAccessIterator2,
         class RandomAccessIterator3,
         class Compare = std::less<>>
void partial_sort(RandomAccessIterator1 first,
                  RandomAccessIterator2 middle,
                  RandomAccessIterator3 last,
                  Compare comp = Compare())
{
  require_random_access_range<RandomAccessIterator1, RandomAccessIterator2>();
  require_random_access_range<RandomAccessIterator2, RandomAccessIterator3>();

  if (middle == last) return;

  // TODO: not a dumb insertion sort.
  for (; first < middle; first++)
    iter_swap(first, _minimum(first, last, comp));
}

/// 25.4.1.4 partial_sort_copy [partial.sort.copy]
template<class BidirectionalIterator1, class BidirectionalIterator2, class Value>
void _insert(BidirectionalIterator1 first, BidirectionalIterator2 last, Value value)
{
  move( make_reverse_iterator(std::prev(last))
      , first
      , make_reverse_iterator(last)
      );
  *first = std::move(value);
}

// non-standard helper
template<class RandomAccessIterator1,
         class RandomAccessIterator2,
         class Value, class Compare = std::less<>>
RandomAccessIterator1 insert(RandomAccessIterator1 first,
                             RandomAccessIterator1 middle,
                             RandomAccessIterator2 last,
                             Value val,
                             Compare comp = Compare())
{
  // FIXME: This check should not be necessary.
  if (first == middle && middle != last) {
    *middle = std::move(val);
    return ++middle;
  }

  // TODO: do binary search
  auto it = find_if_not(first, middle,
                        [&](auto&& val2) { return comp(val2, val); });
  if (it < middle) {
    if (middle != last)
      middle++;
    _insert(it, middle, std::move(val));
  }

  if (it == middle && middle != last)
    *(middle++) = std::move(val);

  return middle;
}

template<class InputIterator, class Terminator1,
         class RandomAccessIterator, class Terminator2,
         class Compare = std::less<>>
RandomAccessIterator
partial_sort_copy(InputIterator first, Terminator1 last,
                  RandomAccessIterator result_first,
                  Terminator2 result_last,
                  Compare comp = Compare())
{
  require_input_range<InputIterator, Terminator1>();
  require_random_access_iterator<RandomAccessIterator>();

  using reference = typename std::iterator_traits<InputIterator>::reference;
  using value_type = typename std::iterator_traits<RandomAccessIterator>::value_type;
  static_assert(std::is_convertible<reference, value_type>::value,
                "input range must be convertible to output range");

  RandomAccessIterator result_middle = result_first;
  auto do_insert = [&](auto&& val) { 
    result_middle =
      insert(result_first, result_middle, result_last, val, comp);
  };

  for_each(first, last, do_insert);
  return result_middle;
}

/// 25.4.1.5 is_sorted [is.sorted]
template<class ForwardIterator, class Terminator1,
         class Compare = std::less<>>
ForwardIterator is_sorted_until(ForwardIterator first, Terminator1 last,
                                Compare comp = Compare())
{
  auto adj = adjacent_find(first, last, not_fn(comp));
  return adj != last ? ++adj : adj;
}

template<class ForwardIterator1, class ForwardIterator2,
         class Compare = std::less<>>
bool is_sorted(ForwardIterator1 first, ForwardIterator2 last,
               Compare comp = Compare())
{
  return is_sorted_until(first, last, comp) == last;
}

// TODO: nth_element

/// 25.4.3 Binary search [alg.binary.search]

///25.4.3.1 lower_bound [lower.bound]
template<class ForwardIterator,
         class T, class Compare = std::less<>>
ForwardIterator
lower_bound(ForwardIterator first, ForwardIterator last,
            const T& value, Compare comp = Compare())
{
  return _lower_bound(first, last, value, comp);
}

///25.4.3.2 upper_bound [upper.bound]
template<class ForwardIterator, class T, class Compare = std::less<>>
ForwardIterator
upper_bound(ForwardIterator first, ForwardIterator last,
            const T& value, Compare comp = Compare())
{
  return _upper_bound(first, last, value, comp);
}

/// 25.4.3.3 equal_range [equal.range]
template<class ForwardIterator, class Terminator,
         class T, class Compare = std::less<>>
std::pair<ForwardIterator, ForwardIterator>
equal_range(ForwardIterator first,
            Terminator last, const T& value,
            Compare comp = Compare())
{
  first = lower_bound(first, last, value, comp);
  return std::make_pair(first, upper_bound(first, last, value, comp));
}

/// 25.4.3.4 binary_search [binary.search]
template<class ForwardIterator, class Terminator,
  class T, class Compare = std::less<>>
bool binary_search(ForwardIterator first, Terminator last,
                   const T& value, Compare comp = Compare())
{
  auto e = lower_bound(first, last, value, comp);
  return e != last && !comp(value, *e);
}

/// 25.4.4 Merge [alg.merge]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class OutputIterator, class Compare = std::less<>>
OutputIterator
merge(InputIterator1 first1, Terminator1 last1,
      InputIterator2 first2, Terminator2 last2,
      OutputIterator result, Compare comp = Compare())
{
  require_io_range<InputIterator1, Terminator1, OutputIterator>();
  require_io_range<InputIterator2, Terminator2, OutputIterator>();
  require_is_predicate<Compare, decltype(*first1), decltype(*first2)>();

  while (first1 != last1 && first2 != last2)
    *result++ = comp(*first1, *first2) ? *first1++ : *first2++;
  return copy(first2, last2, copy(first1, last1, result));
}

template<class BidirectionalIterator1,
         class BidirectionalIterator2,
         class Terminator,
         class Compare = std::less<>>
void inplace_merge(BidirectionalIterator1 first,
                   BidirectionalIterator2 middle,
                   Terminator last,
                   Compare comp = Compare())
{
  require_bidirecitonal_range<BidirectionalIterator1, BidirectionalIterator2>();
  require_forward_range<BidirectionalIterator2, Terminator>();
  require_is_predicate<Compare, decltype(*first), decltype(*middle)>();

  for (; middle != last; first++) {
    if (!comp(*first, *middle)) {
      _insert(first, std::next(middle), std::move(*middle));
      middle++;
    }
  }
}

/// 25.4.5 Set operations on sorted structures [alg.set.operations] 

/// 25.4.5.1 includes [includes]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class Compare = std::less<>>
bool includes(InputIterator1 first1, Terminator1 last1,
              InputIterator2 first2, Terminator2 last2,
              Compare comp = Compare())
{
  require_input_range<InputIterator1, Terminator1>();
  require_input_range<InputIterator2, Terminator2>();
  require_is_predicate<Compare, decltype(*first1), decltype(*first2)>();

  for (; first1 != last1 && first2 != last2; first1++) {
    if (comp(*first2, *first1))
      // first2 < first1, thus for all i from [first1, last1), first2 < i.
      return false;
    else if (!comp(*first1, *first2))
      // first1 == first2
      first2++;
  }
  return first2 == last2;
}

/// 25.4.5.2 set_union [set.union]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class OutputIterator, class Compare = std::less<>>
OutputIterator
set_union(InputIterator1 first1, Terminator1 last1,
          InputIterator2 first2, Terminator2 last2,
          OutputIterator result, Compare comp = Compare())
{
  require_io_range<InputIterator1, Terminator1, OutputIterator>();
  require_io_range<InputIterator2, Terminator2, OutputIterator>();
  require_is_predicate<Compare, decltype(*first1), decltype(*first2)>();

  while (first1 != last1 && first2 != last2)
    if (comp(*first1, *first2))
      *result++ = *first1++;
    else if (comp(*first2, *first1))
      *result++ = *first2++;
    else {
      *result++ = *first2++;
      first1++;
    }
  return copy(first2, last2, copy(first1, last1, result));
}

/// 25.4.5.3 set_intersection [set.intersection]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class OutputIterator, class Compare = std::less<>>
OutputIterator
set_intersection(InputIterator1 first1, Terminator1 last1,
                 InputIterator2 first2, Terminator2 last2,
                 OutputIterator result, Compare comp = Compare())
{
  require_io_range<InputIterator1, Terminator1, OutputIterator>();
  require_io_range<InputIterator2, Terminator2, OutputIterator>();
  require_is_predicate<Compare, decltype(*first1), decltype(*first2)>();

  while (first1 != last1 && first2 != last2)
    if (comp(*first1, *first2))
      first1++;
    else if (comp(*first2, *first1))
      first2++;
    else {
      *result++ = *first2++;
      first1++;
    }
  return result;
}

/// 25.4.5.4 set_difference [set.difference]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class OutputIterator, class Compare = std::less<>>
OutputIterator
set_difference(InputIterator1 first1, Terminator1 last1,
               InputIterator2 first2, Terminator2 last2,
               OutputIterator result, Compare comp = Compare())
{
  require_io_range<InputIterator1, Terminator1, OutputIterator>();
  require_io_range<InputIterator2, Terminator2, OutputIterator>();
  require_is_predicate<Compare, decltype(*first1), decltype(*first2)>();

  while (first1 != last1 && first2 != last2)
    if (comp(*first1, *first2))
      *result++ = *first1++;
    else if (comp(*first2, *first1))
      first2++;
    else {
      first2++;
      first1++;
    }
  return copy(first1, last1, result);
}

/// 25.4.5.5 set_symmetric_difference [set.symmetric.difference]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class OutputIterator, class Compare = std::less<>>
OutputIterator
set_symmetric_difference(InputIterator1 first1, Terminator1 last1,
                         InputIterator2 first2, Terminator2 last2,
                         OutputIterator result, Compare comp = Compare())
{
  require_io_range<InputIterator1, Terminator1, OutputIterator>();
  require_io_range<InputIterator2, Terminator2, OutputIterator>();
  require_is_predicate<Compare, decltype(*first1), decltype(*first2)>();

  while (first1 != last1 && first2 != last2)
    if (comp(*first1, *first2))
      *result++ = *first1++;
    else if (comp(*first2, *first1))
      *result++ = *first2++;
    else {
      first2++;
      first1++;
    }
  return copy(first2, last2, copy(first1, last1, result));
}

/// 25.4.6.1 push_heap [push.heap]
template<class RandomAccessIterator,
         class Size1, class Size2,
         class Compare = std::less<>>
void _heap_down(RandomAccessIterator first,
                Size1 start, Size2 stop,
                Compare comp = Compare())
{
  auto root = start;
  while (root * 2 + 1 < stop) {
    auto child = root * 2 + 1;
    auto swap = root;
    if (!comp(first[child], first[swap]))
      swap = child;
    if (child + 1 < stop && !comp(first[child + 1], first[swap]))
      swap = child + 1;
    if (swap == root)
      return;
    else {
      iter_swap(first + swap, first + root);
      root = swap;
    }
  }
}

template<class RandomAccessIterator, class Size,
         class Compare = std::less<>>
void _heapify(RandomAccessIterator first, Size size,
              Compare comp = Compare())
{
  auto i = (size - 2) / 2 + 1;
  while (i > 0)
    _heap_down(first, --i, size, comp);
}

template<class RandomAccessIterator1, class RandomAccessIterator2,
         class Compare = std::less<>>
void push_heap(RandomAccessIterator1 first, RandomAccessIterator2 last,
               Compare comp = Compare())
{
  _heapify(first, last - first, comp);
}

/// 25.4.6.2 pop_heap [pop.heap]
template<class RandomAccessIterator1,
         class RandomAccessIterator2,
         class Compare = std::less<>>
void pop_heap(RandomAccessIterator1 first, RandomAccessIterator2 last,
              Compare comp = Compare())
{
  iter_swap(first, last - 1);
  _heapify(first, last - first - 1, comp);
}

/// 25.4.6.3 make_heap [make.heap]
template<class RandomAccessIterator1, class RandomAccessIterator2,
         class Compare = std::less<>>
void make_heap(RandomAccessIterator1 first, RandomAccessIterator2 last,
               Compare comp = Compare())
{
  _heapify(first, last - first, comp);
}

/// 25.4.6.4 sort_heap [sort.heap]
template<class RandomAccessIterator1, class RandomAccessIterator2,
         class Compare = std::less<>>
void sort_heap(RandomAccessIterator1 first, RandomAccessIterator2 last,
               Compare comp = Compare())
{
  while (first + 1 < last)
    pop_heap(first, last--, comp);
}

/// 25.4.6.5 is_heap [is.heap]
template<class RandomAccessIterator1, class RandomAccessIterator2,
         class Compare = std::less<>>
RandomAccessIterator1 is_heap_until(RandomAccessIterator1 first,
                                    RandomAccessIterator2 last,
                                   Compare comp = Compare())
{
  bool inc_parent = false;
  for (auto parent = first++;  first < last; first++) {
    if (comp(*parent, *first)) return first;
    if (inc_parent) parent++;
    inc_parent = !inc_parent;
  }
  return first;
}

template<class RandomAccessIterator1, class RandomAccessIterator2,
         class Compare = std::less<>>
bool is_heap(RandomAccessIterator1 first, RandomAccessIterator2 last,
             Compare comp = Compare())
{
  return is_heap_until(first, last, comp) == last;
}

/// 25.4.7 Minimum and maximum [alg.min.max]
using std::min;
using std::max;
//using std::minmax;

template<class ForwardIterator, class Terminator,
         class Compare = std::less<>>
ForwardIterator min_element(ForwardIterator first, Terminator last,
                            Compare comp)
{
  return _minimum(first, last, comp);
}

template<class ForwardIterator, class Terminator,
         class Compare = std::less<>>
ForwardIterator max_element(ForwardIterator first, Terminator last,
                            Compare comp)
{
  return _minimum(first, last, not_fn(flip(comp)));
}
template<class ForwardIterator, class Terminator,
         class Compare = std::less<>>
std::pair<ForwardIterator, ForwardIterator>
minmax_element(ForwardIterator first, Terminator last,
               Compare comp = Compare())
{
  auto min = first, max = first;
  for (; first != last; first++) {
    if (comp(*first, *min))
      min = first;
    else if (comp(*max, *first))
      max = first;
  }
  return std::make_pair(min, max);
}

/// 25.4.8 Lexicographical comparison [alg.lex.comparison]
template<class InputIterator1, class Terminator1,
         class InputIterator2, class Terminator2,
         class Compare = std::less<>>
bool
lexicographical_compare(InputIterator1 first1, Terminator1 last1,
                        InputIterator2 first2, Terminator2 last2,
                        Compare comp = Compare())
{
  for(; first1 != last1 && first2 != last2; first1++, first2++) {
    if (comp(*first1, *first2)) return true;
    if (comp(*first2, *first1)) return false;
  }
  return first1 == last1 && first2 != last2;
}

/// 25.4.9 Permutation generators [alg.permutation.generators]
template<class BidirectionalIterator1, class BidirectionalIterator2,
         class Compare = std::less<>>
bool next_permutation(BidirectionalIterator1 first,
                      BidirectionalIterator2 last,
                      Compare comp = Compare())
{
  if (first == last) return false;
  auto i = std::prev(last);
  if (i == first) return false;

  while (true) {
    auto j = i--;

    if (comp(*i, *j)) {
      auto k = last;

      while (!comp(*i, *--k))
      { }

      iter_swap(i, k);
      reverse(j, last);
      return true;
    }

    if (i == first) {
      reverse(first, last);
      return false;
    }
  }

}

template<class BidirectionalIterator1, class BidirectionalIterator2,
         class Compare = std::less<>>
bool prev_permutation(BidirectionalIterator1 first,
                      BidirectionalIterator2 last,
                      Compare comp = Compare())
{
  if (first == last) return false;
  auto i = std::prev(last);
  if (i == last) return false;

  while (i != first) {
    auto j = i--;

    if (comp(*j, *i)) {
      auto k = last;
      while (!comp(*i, *--k))
      { }

      iter_swap(i, k);
      reverse(j, last);
      return true;
    }
  }

  reverse(first, last);
  return false;
}

}  // namespace alt
