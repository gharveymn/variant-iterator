# variant-iterator

A typesafe union of iterator types leveraging std::variant (thus requiring C++17). 
The header also includes value_iterator which is both an iterator and a container type 
so that one can "iterate" over a single value. The point of this is to form a union between a 
standard library iterator and a value_iterator, thus allowing for uniform polymorphic syntax.

I am aware the tests are really terrible; I'll get around to it. This is still just a 
hack at this point.

## Example
##### source
```c++
#include <iostream>
#include <variant-iterator.hpp>

using namespace gch;

using vector_iter  = std::vector<int *>::iterator;
using value_iter   = value_iterator<int *>;
using variant_iter = variant_iterator<vector_iter, value_iter>;

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

int main (void)
{
  int x = 0, y = 1, z = 3;

  derived1 vect_cont {&x, &y};
  derived2 val_cont  {&z};

  print (vect_cont);
  print (val_cont);  
}
```
##### output
```text
0
1
3
```

Hence, we create a type erasure on the respective iterator types of the derived classes so
that we don't need to neither add specializations nor intrusively add another virtual member function.

## License

I don't recommend using this at the moment, but the license is MIT if you want to.
