/** union-iterator.hpp
 * Short description here. 
 * 
 * Copyright © 2019 Gene Harvey
 *
 * This software may be modified and distributed under the terms
 * of the MIT license. See the LICENSE file for details.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifndef UNION_ITERATOR_TEST_UNION_ITERATOR_HPP
#define UNION_ITERATOR_TEST_UNION_ITERATOR_HPP

#include <cstdint>
#include <type_traits>
#include <variant>
#include <optional_ref.hpp>

#if __has_cpp_attribute(nodiscard) >= 201603L
#  define GCH_NODISCARD [[nodiscard]]
#else
#  define GCH_NODISCARD
#endif

#if __cpp_lib_addressof_constexpr >= 201603L
#  define GCH_CONSTEXPR_ADDRESSOF constexpr
#else
#  define GCH_CONSTEXPR_ADDRESSOF
#endif

#if __cpp_inline_variables >= 201606
#  define GCH_INLINE_VARS inline
#else
#  define GCH_INLINE_VARS
#endif

#if __cpp_lib_constexpr_algorithms >= 201806L
#  define GCH_CONSTEXPR_SWAP constexpr
#else
#  define GCH_CONSTEXPR_SWAP
#endif

#if __cpp_constexpr >= 201304L
#  define GCH_CPP14_CONSTEXPR constexpr
#else
#  define GCH_CPP14_CONSTEXPR
#endif

namespace gch
{

  namespace detail
  {
    template <typename T, typename ...Ts>
    struct all_same : std::conjunction<std::is_same<T, Ts>...>
    { };
    
    template <std::size_t I, typename T, typename ...Ts>
    struct select
        : select<I - 1, Ts...>
    { };
    
    template <typename T, typename ...Ts>
    struct select<0, T, Ts...>
    {
      using type = T;
    };
    
    template <std::size_t I, typename T, typename Head, typename ...Tail>
    struct index : detail::index<I + 1, T, Tail...>
    { };
    
    template <std::size_t I, typename T, typename ...Rest>
    struct index<I, T, T, Rest...>
        : std::integral_constant<std::size_t, I>
    { };
    
    template <std::size_t I, typename T>
    struct index<I, T, T>
        : std::integral_constant<std::size_t, I>
    { };
    
    template <std::size_t I, typename T, typename U>
    struct index<I, T, U>
    { };
    
    template <typename T, typename = void, typename ...Ts>
    struct is_element : std::false_type
    { };
    
    template <typename T, typename ...Ts>
    struct is_element<T, std::void_t<typename index<0, T, Ts...>::type>,
                      Ts...>
        : std::true_type
    { };
    
    template <typename It, typename = void>
    struct is_iterator : std::false_type
    { };
    
    template <typename It>
    struct is_iterator<It, std::void_t<std::iterator_traits<It>>>
        : std::true_type
    { };
  }
  
  template <typename ...Ts>
  using all_same    = detail::all_same<Ts...>;
  
  template <std::size_t I, typename ...Ts>
  using select      = detail::select<I, Ts...>;
  
  template <std::size_t I, typename ...Ts>
  using select_t    = typename select<I, Ts...>::type;
  
  template <typename T, typename ...Ts>
  using index       = detail::index<0, T, Ts...>;
  
  template <typename T, typename ...Ts>
  using is_element  = detail::is_element<T, void, Ts...>;
  
  template <typename It>
  using is_iterator = detail::is_iterator<It>;
  
  template <typename ...Ts>
  inline constexpr bool        all_same_v    = all_same<Ts...>::value;
  
  template <typename T, typename ...Ts>
  inline constexpr std::size_t index_v       = index<T, Ts...>::value;
  
  template <typename T, typename ...Ts>
  inline constexpr std::size_t is_element_v  = is_element<T, Ts...>::value;
  
  template <typename It>
  inline constexpr bool        is_iterator_v = is_iterator<It>::value;
  
  // basically just an iterator-compatible wrapper for a pointer
  // to avoid UB of incrementing a non-associated pointer.
  template <typename Value>
  class reference_iterator
  {
  public:
    
    using difference_type   = int8_t;
    using value_type        = std::remove_cv_t<Value>;
    using pointer           = Value *;
    using reference         = Value&;
    using iterator_category = std::random_access_iterator_tag;
  
    friend reference_iterator<std::add_const_t<Value>>;
    friend reference_iterator<std::add_volatile_t<Value>>;
    friend reference_iterator<std::add_cv_t<Value>>;
  
    reference_iterator            (void)                          = default;
    reference_iterator            (const reference_iterator&)     = default;
    reference_iterator            (reference_iterator&&) noexcept = default;
    reference_iterator& operator= (const reference_iterator&)     = default;
    reference_iterator& operator= (reference_iterator&&) noexcept = default;
    ~reference_iterator           (void)                          = default;
  
    GCH_CONSTEXPR_ADDRESSOF explicit reference_iterator (reference ref)
      : m_ref    (ref),
        m_is_end (false)
    { }
    
    // convert from value_iterator to const_value_iterator
    template<typename NonConst, 
             typename = std::enable_if_t<std::is_same<
               std::remove_cv_t<Value>, NonConst>::value>>
    constexpr 
    reference_iterator (const reference_iterator<NonConst>& it) noexcept
      : m_ref    (it.m_ref),
        m_is_end (it.m_is_end)
    { }
  
    constexpr reference_iterator& operator++ (void) noexcept
    {
      m_is_end = true;
      return *this;
    }
  
    constexpr reference_iterator operator++ (int) noexcept
    {
      reference_iterator save = *this;
      ++*this;
      return save;
    }
  
    constexpr reference_iterator& operator-- (void) noexcept
    {
      m_is_end = false;
      return *this;
    }
  
    constexpr reference_iterator operator-- (int) noexcept
    {
      reference_iterator save = *this;
      --*this;
      return save;
    }
  
    constexpr reference_iterator& operator+= (difference_type n) noexcept
    {
      if (n > 0)
        return ++*this;
      else if (n < 0)
        return --*this;
      return *this;
    }
    
    [[nodiscard]]
    constexpr reference_iterator operator+ (difference_type n) const noexcept
    {
      return reference_iterator (*this) += n;
    }
    
    constexpr reference_iterator& operator-= (difference_type n) noexcept
    {
      return operator+= (-n);
    }
  
    [[nodiscard]]
    constexpr reference_iterator operator- (difference_type n) const noexcept
    {
      return operator+ (-n);
    }
    
    [[nodiscard]]
    constexpr difference_type 
    operator- (const reference_iterator& other) const noexcept
    {
      return m_is_end - other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr reference operator[] (difference_type n) const
    {
      return *(operator+ (n));
    }
    
    //
    // The below functions will be wrong if other does not refer to the same 
    // object. However, this should be fine because it works the same way
    // for std container iterators.
    //
  
    [[nodiscard]]
    constexpr bool operator< (const reference_iterator& other) const noexcept
    {
      return ! m_is_end && other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr bool operator> (const reference_iterator& other) const noexcept
    {
      return m_is_end && ! other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr bool operator<= (const reference_iterator& other) const noexcept
    {
      return m_is_end ? other.m_is_end : true;
    }
  
    [[nodiscard]]
    constexpr bool operator>= (const reference_iterator& other) const noexcept
    {
      return m_is_end ? true : ! other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr bool operator== (const reference_iterator& other) const noexcept
    {
      return (m_ref == other.m_ref) && (m_is_end == other.m_is_end);
    }
  
    [[nodiscard]]
    constexpr bool operator!= (const reference_iterator& other) const noexcept
    {
      return ! operator== (other);
    }
  
    [[nodiscard]]
    constexpr reference operator* (void) const
    {
      return *m_ref;
    }
  
    [[nodiscard]]
    constexpr pointer operator-> (void) const
    {
      return m_ref.get_pointer ();
    }
  
  private:
  
    [[nodiscard]]
    constexpr bool is_end (void) const noexcept
    {
      return m_is_end;
    }
    
    optional_ref<reference> m_ref;
    bool m_is_end = true;
    
  };
  
  template <typename Value> [[nodiscard]]
  constexpr reference_iterator<Value>
  operator+ (typename reference_iterator<Value>::difference_type n, 
             const reference_iterator<Value>& it) noexcept
  {
    return it + n;
  }
  
  template <typename Value>
  using const_reference_iterator = reference_iterator<const Value>;
  
  template <typename Value>
  using reverse_reference_iterator
    = std::reverse_iterator<reference_iterator<Value>>;
  
  template <typename Value>
  using const_reverse_reference_iterator
    = std::reverse_iterator<const_reference_iterator<Value>>;
  
  template <typename Value>
  using ref_iter = reference_iterator<Value>;
  
  template <typename Value>
  using ref_citer = const_reference_iterator<Value>;
  
  template <typename Value>
  using ref_riter = reverse_reference_iterator<Value>;
  
  template <typename Value>
  using ref_criter = const_reverse_reference_iterator<Value>;
  
  static_assert(std::is_trivially_copy_constructible<ref_iter<void *>>::value);
  static_assert(std::is_trivially_move_constructible<ref_iter<void *>>::value);
  static_assert(std::is_trivially_copy_assignable<ref_iter<void *>>::value);
  static_assert(std::is_trivially_move_assignable<ref_iter<void *>>::value);
  static_assert(std::is_trivially_destructible<ref_iter<void *>>::value);
  
  static_assert(std::is_trivially_copy_constructible<ref_citer<void *>>::value);
  static_assert(std::is_trivially_move_constructible<ref_citer<void *>>::value);
  static_assert(std::is_trivially_copy_assignable<ref_citer<void *>>::value);
  static_assert(std::is_trivially_move_assignable<ref_citer<void *>>::value);
  static_assert(std::is_trivially_destructible<ref_citer<void *>>::value);
  
  template <typename Value>
  class value_iterator
  {
  public:
    
    using difference_type   = int8_t;
    using value_type        = std::remove_cv_t<Value>;
    using pointer           = Value *;
    using reference         = Value&;
    using iterator_category = std::random_access_iterator_tag;
  
    struct dereference_exception : std::exception
    {
      dereference_exception (void) = default;
    
      explicit dereference_exception (const char *str)
        : m_str (str)
      { }
    
      [[nodiscard]]
      const char* what (void) const noexcept override
      {
        return m_str;
      }
    private:
      const char* m_str = "Tried to dereference an incremented iterator.";
    };
  
    friend value_iterator<std::add_const_t<Value>>;
    friend value_iterator<std::add_volatile_t<Value>>;
    friend value_iterator<std::add_cv_t<Value>>;
  
    value_iterator            (void)                      = default;
    value_iterator            (const value_iterator&)     = default;
    value_iterator            (value_iterator&&) noexcept = default;
    value_iterator& operator= (const value_iterator&)     = default;
    value_iterator& operator= (value_iterator&&) noexcept = default;
    ~value_iterator           (void)                      = default;
  
    template <typename ...Args, 
              typename = std::enable_if_t<
                std::is_constructible<value_type, Args...>::value>>
    constexpr explicit value_iterator (Args&&... args)
        : m_value (std::forward<Args> (args)...),
          m_is_end (false)
    { }
  
    // convert from value_iterator to const_value_iterator
    template<typename NonConst,
             typename = std::enable_if_t<
               std::is_same<std::remove_const_t<Value>, NonConst>::value>>
    constexpr value_iterator (const value_iterator<NonConst>& it) noexcept (
                         std::is_nothrow_copy_constructible<value_type>::value)
      : m_value  (it.m_value),
        m_is_end (it.m_is_end)
    { }
  
    // move from value_iterator to const_value_iterator
    template<typename NonConst,
             typename = std::enable_if_t<
                 std::is_same<std::remove_const_t<Value>, NonConst>::value>>
    constexpr value_iterator (value_iterator<NonConst>&& it) noexcept (
    std::is_nothrow_move_constructible<value_type>::value)
        : m_value  (std::move (it.m_value)),
          m_is_end (it.m_is_end)
    { }
  
    constexpr value_iterator& operator++ (void) noexcept
    {
      m_is_end = true;
      return *this;
    }
  
    constexpr value_iterator operator++ (int) noexcept
    {
      value_iterator save = *this;
      ++*this;
      return save;
    }
  
    constexpr value_iterator& operator-- (void) noexcept
    {
      m_is_end = false;
      return *this;
    }
  
    constexpr value_iterator operator-- (int) noexcept
    {
      value_iterator save = *this;
      --*this;
      return save;
    }
  
    constexpr value_iterator& operator+= (difference_type n) noexcept
    {
      m_is_end = additive_result (n);
      return *this;
    }
  
    [[nodiscard]]
    constexpr value_iterator operator+ (difference_type n) const noexcept
    {
      return value_iterator(*this) += n;
    }
    
    constexpr value_iterator& operator-= (difference_type n) noexcept
    {
      return operator+= (-n);
    }
  
    [[nodiscard]]
    constexpr value_iterator
    operator- (difference_type n) const noexcept
    {
      return operator+ (-n);
    }
  
    [[nodiscard]]
    constexpr difference_type 
    operator- (const value_iterator& other) const noexcept
    {
      return m_is_end - other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr reference operator[] (difference_type n) const
    {
      if (additive_result (n))
        throw dereference_exception ();
      return m_value;
    }
    
    //
    // The below functions will be wrong if other does not refer to the same 
    // object. However, this should be fine because it works the same way
    // for std container iterators.
    //
  
    [[nodiscard]]
    constexpr bool operator< (const value_iterator& other) const noexcept
    {
      return ! m_is_end && other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr bool operator> (const value_iterator& other) const noexcept
    {
      return m_is_end && ! other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr bool operator<= (const value_iterator& other) const noexcept
    {
      return m_is_end ? other.m_is_end : true;
    }
  
    [[nodiscard]]
    constexpr bool operator>= (const value_iterator& other) const noexcept
    {
      return m_is_end ? true : ! other.m_is_end;
    }
  
    [[nodiscard]]
    constexpr bool operator== (const value_iterator& other) const noexcept
    {
      return (m_value == other.m_value) && (m_is_end == other.m_is_end);
    }
  
    [[nodiscard]]
    constexpr bool operator!= (const value_iterator& other) const noexcept
    {
      return ! operator== (other);
    }
  
    [[nodiscard]]
    constexpr reference operator* (void) const
    {
      if (m_is_end)
        throw dereference_exception ();
      return m_value;
    }
  
    [[nodiscard]]
    constexpr pointer operator-> (void) const
    {
      return m_is_end ? nullptr : &m_value;
    }
  
  private:
    
    [[nodiscard]]
    constexpr bool additive_result (difference_type n) const noexcept 
    {
      return n == 0 ? m_is_end : n > 0;
    }
  
    mutable value_type m_value  = value_type ();
            bool       m_is_end = true;
    
  };

  template <typename Value> [[nodiscard]]
  constexpr value_iterator<Value>
  operator+ (typename value_iterator<Value>::difference_type n,
             const value_iterator<Value>& it) noexcept
  {
    return it + n;
  }
  
  template <typename Value>
  using const_value_iterator = value_iterator<const Value>;
  
  template <typename Value>
  using reverse_value_iterator
    = std::reverse_iterator<value_iterator<Value>>;
  
  template <typename Value>
  using const_reverse_value_iterator
    = std::reverse_iterator<const_value_iterator<Value>>;
  
  template <typename Value>
  using value_iter   = value_iterator<Value>;
  
  template <typename Value>
  using value_citer  = const_value_iterator<Value>;
  
  template <typename Value>
  using value_riter  = reverse_value_iterator<Value>;
  
  template <typename Value>
  using value_criter = const_reverse_value_iterator<Value>;

  // static_assert(std::is_trivially_default_constructible<value_iter<void *>>::value);
  static_assert(std::is_trivially_copy_constructible<value_iter<void *>>::value);
  static_assert(std::is_trivially_move_constructible<value_iter<void *>>::value);
  static_assert(std::is_trivially_copy_assignable<value_iter<void *>>::value);
  static_assert(std::is_trivially_move_assignable<value_iter<void *>>::value);
  static_assert(std::is_trivially_destructible<value_iter<void *>>::value);
  
  static_assert(std::is_trivially_copy_constructible<value_citer<void *>>::value);
  static_assert(std::is_trivially_move_constructible<value_citer<void *>>::value);
  static_assert(std::is_trivially_copy_assignable<value_citer<void *>>::value);
  static_assert(std::is_trivially_move_assignable<value_citer<void *>>::value);
  static_assert(std::is_trivially_destructible<value_citer<void *>>::value);
  
  template <typename T>
  value_iter<T> value_begin (T&& val)
  {
    return value_iter<T> (std::forward<T> (val));
  }
  
  template <typename T>
  value_iter<T> value_end (T&& val)
  {
    return ++value_begin (std::forward<T> (val));
  }
  
  template <class... Functors> 
  struct overload : Functors... 
  { 
    using Functors::operator()...; 
  };
  
  template <class... Functors>
  overload (Functors...) -> overload<Functors...>;
  
  template <typename ...Its>
  class variant_iterator
  {
    static_assert (std::conjunction<is_iterator<Its>...>::value, 
                   "All template arguments must be iterators.");
  
    template <typename It>
    using diff_t = typename std::iterator_traits<It>::difference_type;
  
    template <typename It>
    using value_t = typename std::iterator_traits<It>::value_type;
  
    template <typename It>
    using pointer_t = typename std::iterator_traits<It>::pointer;
  
    template <typename It>
    using reference_t = typename std::iterator_traits<It>::reference;
  
    template <typename It>
    using category_t = typename std::iterator_traits<It>::iterator_category;

  public:
  
    template <std::size_t I = 0>
    using iterator_type = select_t<I, Its...>;
  
    static_assert (all_same<value_t<Its>...>::value,
                   "value types must be equal.");
  
    static_assert (all_same<reference_t<Its>...>::value,
                   "reference types must be equal.");
  
    static_assert (all_same<pointer_t<Its>...>::value,
                   "pointer types must be equal.");
  
    using difference_type   = std::common_type_t<diff_t<Its>...>;
    using value_type        = value_t<iterator_type<>>;
    using pointer           = pointer_t<iterator_type<>>;
    using const_pointer     = const value_type *;
    using reference         = reference_t<iterator_type<>>;
    using const_reference   = const value_type&;
    using iterator_category = std::common_type_t<category_t<Its>...>;
  
    struct type_exception : std::exception
    {
      type_exception (void) = default;
      
      // explicit type_exception (const char *str)
      //   : m_str (str)
      // { }
    
      [[nodiscard]]
      const char* what (void) const noexcept override
      {
        return m_str;
      }
    private:
      const char* m_str = "Iterator types are not the same.";
    };

  private:
  
    struct deduced_tag;
  
    template <template<typename...> class BinaryOperator, 
              typename Return = deduced_tag>
    struct binary_visitor : BinaryOperator<>
    {
      template <typename It>
      constexpr Return operator() (It&& lhs, It&& rhs) const
      {
        return Return (BinaryOperator<>::operator() (std::forward<It> (lhs), 
                                                     std::forward<It> (rhs)));
      }
    
      template <typename T, typename U, 
                typename = std::enable_if_t<! std::is_same<T, U>::value>>
      constexpr Return operator() (T&&, U&&) { throw type_exception (); }
    };
  
    template <template<typename...> class BinaryOperator>
    struct binary_visitor<BinaryOperator, deduced_tag> : BinaryOperator<>
    {
      using BinaryOperator<>::operator();
      using return_type = std::invoke_result_t<BinaryOperator<>,
                                               iterator_type<>, 
                                               iterator_type<>>;
    
      static_assert (all_same<std::invoke_result_t<BinaryOperator<void>,
                                                   Its, Its>...>::value,
                     "Binary operator return types must be uniform.");
    
      template <typename T, typename U,
                typename = std::enable_if_t<! std::is_same<T, U>::value>>
      constexpr return_type operator() (T&&, U&&) { throw type_exception (); }
    };
    
  public:
  
    variant_iterator            (void)                        = default;
    variant_iterator            (const variant_iterator&)     = default;
    variant_iterator            (variant_iterator&&) noexcept = default;
    variant_iterator& operator= (const variant_iterator&)     = default;
    variant_iterator& operator= (variant_iterator&&) noexcept = default;
    ~variant_iterator           (void)                        = default;
  
    // value copying initializer
    template <typename It,
              typename = std::enable_if_t<is_element<It, Its...>::value>>
    constexpr explicit variant_iterator (const It& it)
        : m_variant (it)
    { }
    
    template <typename It, 
              typename = std::enable_if_t<is_element<It, Its...>::value>>
    constexpr explicit variant_iterator (It&& it)
      : m_variant (std::forward<It> (it))
    { }
  
    constexpr variant_iterator& operator++ (void) noexcept
    {
      std::visit ([] (auto&& it) -> void { ++it; }, m_variant);
      return *this;
    }
  
    constexpr variant_iterator operator++ (int) noexcept
    {
      return std::visit ([] (auto&& it) -> variant_iterator 
                         { return variant_iterator (it++); }, m_variant);
    }
  
    constexpr variant_iterator& operator-- (void) noexcept
    {
      std::visit ([] (auto&& it) -> void { --it; }, m_variant);
      return *this;
    }
  
    constexpr variant_iterator operator-- (int) noexcept
    {
      return std::visit ([] (auto&& it) -> variant_iterator
                         { return variant_iterator (it--); }, m_variant);
    }
  
    constexpr variant_iterator& operator+= (difference_type n) noexcept
    {
      std::visit ([&n] (auto&& it) { it += n; }, m_variant);
      return *this;
    }
  
    [[nodiscard]]
    constexpr variant_iterator operator+ (difference_type n) const noexcept
    {
      return std::visit ([&n] (auto&& it) -> variant_iterator
                         { return variant_iterator (it + n); }, m_variant);
    }
  
    [[nodiscard]]
    friend constexpr variant_iterator operator+ (difference_type n,
                                       const variant_iterator& var) noexcept
    {
      return std::visit ([&n] (auto&& it) -> variant_iterator
                         { return variant_iterator<Its...> (n + it); }, 
                         var.m_variant);
    }
  
    constexpr variant_iterator& operator-= (difference_type n) noexcept
    {
      std::visit ([&n] (auto&& it) -> void { it -= n; }, m_variant);
      return *this;
    }
  
    [[nodiscard]]
    constexpr variant_iterator operator- (difference_type n) const noexcept
    {
      return std::visit ([&n] (auto&& it) -> variant_iterator
                         { return variant_iterator (it - n); }, m_variant);
    }
  
    [[nodiscard]]
    constexpr difference_type operator- (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::minus, difference_type> { },
                         m_variant, other.m_variant);
    }
  
    [[nodiscard]]
    constexpr reference operator[] (difference_type n) const
    {
      return std::visit ([&n] (auto&& it) -> reference
                         { return it[n]; },  m_variant);
    }
  
    [[nodiscard]]
    constexpr bool operator< (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::less> { },
                         m_variant, other.m_variant);
    }
  
    [[nodiscard]]
    constexpr bool operator> (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::greater> { },
                         m_variant, other.m_variant);
    }
  
    [[nodiscard]]
    constexpr bool operator<= (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::less_equal> { },
                         m_variant, other.m_variant);
    }
  
    [[nodiscard]]
    constexpr bool operator>= (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::greater_equal> { },
                         m_variant, other.m_variant);
    }
  
    [[nodiscard]]
    constexpr bool operator== (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::equal_to> { },
                         m_variant, other.m_variant);
    }
  
    [[nodiscard]]
    constexpr bool operator!= (const variant_iterator& other) const
    {
      return std::visit (binary_visitor<std::not_equal_to> { },
                         m_variant, other.m_variant);
    }
  
    [[nodiscard]]
    constexpr reference operator* (void) const
    {
      return std::visit ([] (auto&& it) -> reference
                         { return it.operator* (); }, m_variant);
    }
  
    [[nodiscard]]
    constexpr pointer operator-> (void) const
    {
      return std::visit ([] (auto&& it) -> pointer
                         { return it.operator-> (); }, m_variant);
    }
  
  private:
    
    std::variant<Its...> m_variant;
    
  };  
}

#undef GCH_NODISCARD
#undef GCH_CONSTEXPR_ADDRESSOF
#undef GCH_INLINE_VARS
#undef GCH_CONSTEXPR_SWAP
#undef GCH_CPP14_CONSTEXPR

#endif /* UNION_ITERATOR_TEST_UNION_ITERATOR_HPP */
