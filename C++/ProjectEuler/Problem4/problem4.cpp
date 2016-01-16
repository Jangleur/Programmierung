// Palindrom-Tester
// -------------------------

#include<iostream>
using namespace std;

int laenge(int);
bool palindrom(int);

int main() {
  
  int x;
  cout << "Geben Sie eine Zahl ein: " << endl;
  cin >> x;
  
  if (palindrom(x))
    cout << "Palindrom" << endl;
  else
    cout << "Kein Palindrom " << endl;
  return 0;
 
 
 cout << laenge(x) << endl;
 palindrom(x);
 return 0;
 
}

int laenge(int x)
{
  int z = 0;
  do {
    if (!((x%10) == 0))
    {
      x = x - (x%10);
      z++;
    } else
      x = x/10;
  } while ( x != 0);
  return z;
}

bool palindrom(int x)
{
  int l = laenge(x);
  int a[l];
  for (int i = 0; i < l; i++)
  {
    if (!((x%10) == 0)) 
    {
      a[i] = x%10;
      x = x - (x%10);
    } else {
      x = x/10;
      a[i] = x%10;
      x = x - (x%10);
    }
  }
  for (int k = 0, j = l-1;  k < l && j >= 0 ; k++, j--)
  {
    if (a[k] != a[j])
      return false;
  }
  return true;
}
