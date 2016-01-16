// Project Euler Problem 3
/* The prime factors of 13195 are 5, 7, 13 and 29.
 * What is the largest prime factor of the number 600851475143 ?
*/
// --------------------------------------------------------------

#include<iostream>
using namespace std;

bool prim(double);
double primfak(double);

int main() 
{
  long n = 600851475143;
  cout << primfak(n) << endl;

  return 0;
}

bool prim(double n)
{
  if (n == 2)
    return true;
  double i = 2;
  while (i < n/2)
  {
    if ((n %  i) == 0)
      return false;
    i++;
  }
  return true;
}

double primfak(double n)
{
  double i = n-1;
  while (i > 0)
  {
    if (prim(i) && (n%i == 0))
      return i;
    i--;
  }
}