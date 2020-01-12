#ifdef _ITERATOR_DEBUG_LEVEL
#undef _ITERATOR_DEBUG_LEVEL
#endif
#define _ITERATOR_DEBUG_LEVEL 0

#include <iostream>
#include <vector>
#include <memory>
#include <random>
#include <chrono>
#include <functional>
#include <cassert>

#include <variant-iterator.hpp>

std::random_device rd;
std::mt19937 rd_gen (rd ());
std::uniform_real_distribution<> dist (0.0, 1.0);

struct my_struct
{
  my_struct operator+ (const my_struct& other) const
  {
    return { x + other.x, y + other.y, z + other.z };
  }
  unsigned x;
  unsigned y;
  unsigned z;
};

unsigned gen_int (void)
{
  return static_cast<unsigned> (dist (rd_gen) * double (UINT_MAX));
}

my_struct gen_my_struct (void)
{
  return { gen_int (), gen_int (), gen_int () };
}

using vect_iter = std::vector<my_struct *>::iterator;
using vect_citer = std::vector<my_struct *>::const_iterator;
using vect_riter = std::vector<my_struct *>::reverse_iterator;
using vect_criter = std::vector<my_struct *>::const_reverse_iterator;

using val_iter = gch::value_iterator<my_struct *>;
using val_citer = gch::const_value_iterator<my_struct *>;
using val_riter = gch::reverse_value_iterator<my_struct *>;
using val_criter = gch::const_reverse_value_iterator<my_struct *>;

struct my_container
{  
  my_container (int size)
    : m_uptr (std::make_unique<my_struct> (gen_my_struct ())),
      m_ptr (m_uptr.get ())
  {
    m_single_uvect.emplace_back (
      std::make_unique<my_struct> (gen_my_struct ()));
    m_single_vect.emplace_back (m_single_uvect.back ().get ());
    
    for (int i = 0; i < size; ++i)
    {
      gen ();
    }
  }
  
  void gen (void)
  {
    std::unique_ptr<my_struct>& uptr 
      = m_uvect.emplace_back (std::make_unique<my_struct> (gen_my_struct ()));
    m_vect.emplace_back (uptr.get ());
  }
  
  static constexpr double dist_partition = 0.1;
  
  template <typename UnionIterator>
  UnionIterator get (void)
  {
    using iter = UnionIterator;
    if (dist (rd_gen) < dist_partition)
      return iter(typename iter::template iterator_type<0> (get_vector_iter ()));
    else
      return iter (typename iter::template iterator_type<1> (get_ptr_iter<> ()));
  }
  
  using value_type = my_struct *;
  
  value_type& get_ref (void)
  {
    if (dist (rd_gen) < dist_partition)
      return *get_vector_iter ();
    else
      return m_ptr;
  }
  
  template <typename Return = val_iter, typename Cast = val_iter>
  Return get_ptr_iter (void)
  {
    return Return (Cast (val_iter (m_ptr)));
  }
  
  vect_iter get_vector_iter (void)
  {
    return m_vect.begin () + (int)(dist (rd_gen) * m_vect.size ());
  }
  
private:
  
  std::unique_ptr<my_struct> m_uptr;
  std::vector<std::unique_ptr<my_struct>> m_uvect;
  std::vector<std::unique_ptr<my_struct>> m_single_uvect;
  
  my_struct *m_ptr;
  std::vector<my_struct *> m_vect;
  std::vector<my_struct *> m_single_vect;
  
};

template <>
auto my_container::get<vect_iter> (void) -> vect_iter
{
  using iter = vect_iter;
  if (dist (rd_gen) < dist_partition)
    return get_vector_iter ();
  else
    return m_single_vect.begin ();
}

template <>
gch::variant_iterator<vect_riter, val_riter>
my_container::get_ptr_iter<gch::variant_iterator<vect_riter, val_riter>, val_riter> (void)
{
  return gch::variant_iterator<vect_riter, val_riter> (val_riter (++val_iter (m_ptr)));
}

template <>
gch::variant_iterator<vect_criter, val_criter>
my_container::get_ptr_iter<gch::variant_iterator<vect_criter, val_criter>, val_criter> (void)
{
  return gch::variant_iterator<vect_criter, val_criter> (val_criter (++val_iter (m_ptr)));
}

template <>
auto my_container::get_ptr_iter<vect_iter> (void) -> vect_iter
{
  return m_single_vect.begin ();
}

template <>
auto my_container::get_ptr_iter<vect_citer> (void) -> vect_citer
{
  return m_single_vect.begin ();
}

template <bool reverse>
struct my_container1
{
  my_container1 (int size)
    : m_uptr (std::make_unique<my_struct> (gen_my_struct ())),
      m_ptr (m_uptr.get ())
  {
    m_single_uvect.emplace_back (
      std::make_unique<my_struct> (gen_my_struct ()));
    m_single_vect.emplace_back (m_single_uvect.back ().get ());
    
    for (int i = 0; i < size; ++i)
    {
      gen ();
    }
  }
  
  void gen (void)
  {
    std::unique_ptr<my_struct>& uptr
      = m_uvect.emplace_back (std::make_unique<my_struct> (gen_my_struct ()));
    m_vect.emplace_back (uptr.get ());
  }
  
  static constexpr double dist_partition = 0.1;
  
  template <typename UnionIterator, typename PtrAs>
  UnionIterator get (void)
  {
    using iter = UnionIterator;
    using It1  = typename iter::template iterator_type<0>;
    using It2  = typename iter::template iterator_type<1>;
    if (dist (rd_gen) < dist_partition)
      return iter(It1 (get_vector_iter ()));
    else
      return iter (get_ptr_iter<It2, It2, PtrAs> ());
  }
  
  using value_type = my_struct *;
  
  value_type& get_ref (void)
  {
    if (dist (rd_gen) < dist_partition)
      return *get_vector_iter ();
    else
      return m_ptr;
  }
  
  template <typename Return, typename Cast, typename As>
  Return get_ptr_iter (void)
  {
    return Return (Cast (As (m_ptr)));
  }
  
  vect_iter get_vector_iter (void)
  {
    return m_vect.begin () + (int)(dist (rd_gen) * m_vect.size ());
  }

private:
  
  std::unique_ptr<my_struct> m_uptr;
  std::vector<std::unique_ptr<my_struct>> m_uvect;
  std::vector<std::unique_ptr<my_struct>> m_single_uvect;
  
  my_struct *m_ptr;
  std::vector<my_struct *> m_vect;
  std::vector<my_struct *> m_single_vect;
  
};

template <>
struct my_container1<true>
{
  my_container1 (int size)
    : m_uptr (std::make_unique<my_struct> (gen_my_struct ())),
      m_ptr (m_uptr.get ())
  {
    m_single_uvect.emplace_back (
      std::make_unique<my_struct> (gen_my_struct ()));
    m_single_vect.emplace_back (m_single_uvect.back ().get ());
    
    for (int i = 0; i < size; ++i)
    {
      gen ();
    }
  }
  
  void gen (void)
  {
    std::unique_ptr<my_struct>& uptr
      = m_uvect.emplace_back (std::make_unique<my_struct> (gen_my_struct ()));
    m_vect.emplace_back (uptr.get ());
  }
  
  static constexpr double dist_partition = 0.1;
  
  template <typename UnionIterator, typename PtrAs>
  UnionIterator get (void)
  {
    using iter = UnionIterator;
    using It1  = typename iter::template iterator_type<0>;
    using It2  = typename iter::template iterator_type<1>;
    if (dist (rd_gen) < dist_partition)
      return iter(It1 (get_vector_iter ()));
    else
      return iter (get_ptr_iter<It2, It2, PtrAs> ());
  }
  
  using value_type = my_struct *;
  
  value_type& get_ref (void)
  {
    if (dist (rd_gen) < dist_partition)
      return *get_vector_iter ();
    else
      return m_ptr;
  }
  
  template <typename Return, typename Cast, typename As>
  Return get_ptr_iter (void)
  {
    return Return (Cast (++As (m_ptr)));
  }
  
  vect_iter get_vector_iter (void)
  {
    return ++m_vect.begin () + (int)(dist (rd_gen) * m_vect.size ());
  }

private:
  
  std::unique_ptr<my_struct> m_uptr;
  std::vector<std::unique_ptr<my_struct>> m_uvect;
  std::vector<std::unique_ptr<my_struct>> m_single_uvect;
  
  my_struct *m_ptr;
  std::vector<my_struct *> m_vect;
  std::vector<my_struct *> m_single_vect;
  
};

constexpr int iterations = 100000;

template <typename Container>
double test_perf_no_union (Container& cont)
{
  using namespace std::chrono;
  using clock = high_resolution_clock;
  using time = clock::time_point;
  time t1 = clock::now ();
  
  for (int i = 0; i < iterations; ++i)
  {
    *cont.get_ref () = *cont.get_ref () + *cont.get_ref ();
  }
  
  time t2 = clock::now ();
  return duration_cast<duration<double>> (t2 - t1).count ();
}

template <typename UnionIterator, typename PtrAs, typename Container>
double test_perf(Container& cont)
{
  using namespace std::chrono;
  using clock = high_resolution_clock;
  using time = clock::time_point;
  time t1 = clock::now ();
  
  using iter = UnionIterator;
  using It1  = typename iter::template iterator_type<0>;
  using It2  = typename iter::template iterator_type<1>;
  
  using fptr = iter (Container::*)(void);
  fptr fp_get = &Container::template get<iter, PtrAs>;  
  auto get = [&fp_get] (Container& cont) -> iter
  {
    return std::invoke (fp_get, cont);
  };
  
  for (int i = 0; i < iterations; ++i)
  {
    iter set_iter = get (cont);
    iter op1_iter = get (cont);
    iter op2_iter = get (cont);
    **set_iter = **op1_iter + **op2_iter;
    
    iter ptr_iter1 = cont.template get_ptr_iter<iter, It2, PtrAs> ();
    iter ptr_iter2 = cont.template get_ptr_iter<iter, It2, PtrAs> ();    
    
    assert (ptr_iter1 == ptr_iter2);
    assert (ptr_iter1 <= ptr_iter2);
    assert (ptr_iter1 >= ptr_iter2);
    assert ((ptr_iter1 += 0) == ptr_iter2);
    
    iter tmp = ptr_iter1;
    assert (ptr_iter1++ == tmp);
    assert (--ptr_iter1 == ptr_iter2);
    ptr_iter1 += 1;
    assert (ptr_iter1 != ptr_iter2);
    assert (ptr_iter1 == (ptr_iter2 + 1));
    assert ((ptr_iter1 - 1) == ptr_iter2);
    assert ((ptr_iter1 - ptr_iter2) == 1);
    assert ((ptr_iter2 - ptr_iter1) == -1);
    
    using std::swap;
    swap (ptr_iter1, ptr_iter2);
    assert (ptr_iter1 < ptr_iter2);
    assert (ptr_iter2 > ptr_iter1);
    assert (ptr_iter1 <= ptr_iter2);
    assert (ptr_iter2 >= ptr_iter1);
    
    assert (++ptr_iter1 == ptr_iter2--);
    assert ((ptr_iter1 -= 1) == ptr_iter2);
  
    assert ((ptr_iter1 - ptr_iter2) == 0);
    assert (ptr_iter1[0] == *ptr_iter2);
    assert ((1 + ptr_iter1) == (1 + ptr_iter2));
    
    const iter const_ptr_iter = ptr_iter1;
    assert (const_ptr_iter[0] == *ptr_iter2);
    auto val_tmp1 = const_ptr_iter.operator-> ();
    
    iter moved (std::move (ptr_iter1));
    
    assert (moved == ptr_iter2);
    
    iter copied (moved);
  
    assert (moved == copied++);
    
    // copy assign
    ptr_iter1 = copied;
  
    assert (++moved == ptr_iter1);
    assert (ptr_iter1 != ptr_iter2);
    
    // move assign
    ptr_iter2 = std::move (copied);
    
    assert (ptr_iter1 == ptr_iter2);
    assert (moved == ptr_iter2);
    
    // init as with a vector iterator, then switch to value_iterator
    iter viter1 (It1 (cont.get_vector_iter ()));
    auto vtmp = It1 (cont.get_vector_iter ());
    iter viter2 (vtmp);
    
//    assert (viter1 != ptr_iter1);
//    assert (viter2 != ptr_iter1);
    static_cast<void> (viter1 - viter2);
    assert (viter1 != iter ());
    assert (iter () != viter2);
    
    viter1 = ptr_iter1;
    assert (viter1 == ptr_iter1);
  
    viter2 = std::move (moved);
    assert (viter2 == ptr_iter1);
    
    auto val_tmp2 = (--viter2).operator-> ();
    
  }
  
  time t2 = clock::now ();
  return duration_cast<duration<double>> (t2 - t1).count ();
}

#include <iostream>
#include <variant-iterator.hpp>

using namespace gch;

using vector_iter  = std::vector<int *>::iterator;
using value_iter1   = value_iterator<int *>;
using variant_iter = variant_iterator<vector_iter, value_iter1>;

struct base
{
  virtual variant_iter begin (void) = 0;
  virtual variant_iter end   (void) = 0;
};

struct derived1 : base
{
  derived1 (std::initializer_list<int *> in) : m_vect (in) { }
  variant_iter begin (void) override { return variant_iter (m_vect.begin ()); }
  variant_iter end   (void) override { return variant_iter (m_vect.end ());   }
private:
  std::vector<int *> m_vect;
};

struct derived2 : base
{
  derived2 (int *val) : m_val (val) { }
  variant_iter begin (void) override { return variant_iter (value_begin (m_val)); }
  variant_iter end   (void) override { return variant_iter (value_end   (m_val)); }
private:
  int *m_val;
};

void print (base& b)
{
  for (int *x : b)
    std::cout << *x << std::endl;
}

int main()
{
  constexpr int cont_size = 10000;  
  my_container1<false> cont (cont_size);
  
  std::cout << "test_perf_no_union (): ";
  std::cout << test_perf_no_union (cont) << std::endl;
  
//  using iter_v1 = uiter::union_iterator<vect_iter>;
//  std::cout << "test_perf<iter_v1> (cont): ";
//  std::cout << test_perf<iter_v1> (cont) << std::endl;
  
//  using iter_v2 = uiter::union_iterator2<vect_iter, uiter::value_iterator<my_struct *>>;
//  std::cout << "test_perf<iter_v2> (cont): ";
//  std::cout << test_perf<iter_v2> (cont) << std::endl;
  
//  using iter_v3 = uiter::union_iterator<vect_citer>;
//  std::cout << "test_perf<iter_v3> (cont): ";
//  std::cout << test_perf<iter_v3> (cont) << std::endl;
  
//  using iter_v4 = uiter::union_iterator2<vect_citer, uiter::const_value_iterator<my_struct *>>;
//  std::cout << "test_perf<iter_v4> (cont): ";
//  std::cout << test_perf<iter_v4> (cont) << std::endl;
  
  using iter_v9 = gch::variant_iterator<vect_iter, val_iter>;
  std::cout << "test_perf<iter_v9> (cont): ";
  std::cout << test_perf<iter_v9, val_iter> (cont) << std::endl;
  
  using iter_v10 = gch::variant_iterator<vect_citer, val_citer>;
  std::cout << "test_perf<iter_v10> (cont): ";
  std::cout << test_perf<iter_v10, val_citer> (cont) << std::endl;
  
  my_container1<true> rev_cont (cont_size);
  
  using iter_v11 = gch::variant_iterator<vect_riter, val_riter>;
  std::cout << "test_perf<iter_v11> (cont): ";
  std::cout << test_perf<iter_v11, val_iter> (rev_cont) << std::endl;
  
  using iter_v12 = gch::variant_iterator<vect_criter, val_criter>;
  std::cout << "test_perf<iter_v12> (cont): ";
  std::cout << test_perf<iter_v12, val_citer> (rev_cont) << std::endl;
  
  using struct_ptr = my_struct *;
  val_iter vit2 = cont.template get_ptr_iter<val_iter, val_iter, val_iter> ();
  struct_ptr& ref = vit2.operator* ();
  struct_ptr *ptr = vit2.operator-> (); 
  
  const val_iter const_vit2 = vit2;
  const struct_ptr& cref = const_vit2.operator* ();
  const struct_ptr *cptr = const_vit2.operator-> ();
  
  val_citer cvit2 (vit2);
  val_citer cvit2_1 (std::move (vit2));
  
  // expected fail
  try
    {
      auto val_tmp3 = vit2[1];
      assert (false);
    }
  catch (...) { }
  
  // expected fail
  ++vit2;
  try
    {
      auto val_tmp3 = vit2[0];
      assert (false);
    }
  catch (...) { }
  
  // expected fail
  try
    {
      auto val_tmp3 = *vit2;
      assert (false);
    }
  catch (...) { }
  
//  using iter_vect = vect_iter;
//  std::cout << "test_perf<iter_vect> (cont): ";
//  std::cout << test_perf<iter_vect> (cont) << std::endl;

  int x = 0, y = 1, z = 3;
  
  derived1 vect_cont {&x, &y};
  derived2 val_cont  {&z};
  
  print (vect_cont);
  print (val_cont);
  
  return 0;
}
