/*Problema 3635 - UVA: https://icpcarchive.ecs.baylor.edu/index.php?option=com_onlinejudge&Itemid=8&category=19&page=show_problem&problem=1636*/


#include <iostream>
#include <cstdio>
#include <stack>
#include <queue>
#include <list>
#include <vector>
#include <map>
#include <utility>
#include <functional>
#include <algorithm>
#include <cmath>
#include <limits>
#define PI 3.141592653589793238463
#define sz(a) int((a).size()) 
#define pb push_back 
#define all(c) (c).begin(),(c).end() 
#define tr(c,i) for(typeof((c).begin()) (i) = (c).begin(); (i) != (c).end(); (i)++) 
#define present(c,x) ((c).find(x) != (c).end()) 
#define cpresent(c,x) (find(all(c),x) != (c).end()) 
#define I(x) ((int)(x))
using namespace std;
typedef vector<int> vi; 
typedef vector<vi > vvi; 
typedef pair<int,int> ii;
typedef pair<double,int> di;
typedef vector<string> vs; 
typedef vector<ii> vii;
typedef vector<di> vdi;
typedef vector< pair<double, ii> > vdii;

template <class T>
int N, F;
double r[10000];
class mycmp: less<T>{
	public:
		bool operator() (const T& e1, const T& e2)
		{
			if(e1.first > e2.first)
			{
				return true;
			}
			if(e1.first == e2.first)
			{
				if(e1.second < e2.second)
				{
					return true;
				}
			}
			return false;
		}
};

bool satisfaz(int a)
{
	int l, c;
	int somatorio = 0;
	for(i = 0; i < N; i++)
	{
		somatorio += I(r[i]/a);
	}
	return somatorio >= F;
}

bool fsatisfaz(int a)
{
	int l, c;
	int somatorio = 0;
	for(i = 0; i < N; i++)
	{
		somatorio += I(r[i]/a);
	}
	return somatorio >= F;
}

inline float satisfaz2(int a, int b)
{
	int i;
	float aux = 0.00000;
	cout << "Aux: " << aux;
	a = a*10000;
	b = b*10000;
	for(i = b; i >= a; i--)
	{
		aux = i;
		if(fsatisfaz(aux/10000.0000F))
		{
			return i;
		}
	}
	return -1;
}

inline float satisfaz3(float a, float b)
{
	int i;
	float aux = 0.0000;
	int c = a*10000;
	int d = b*10000;
	for(i = d; i >= c; i--)
	{
		aux = i;
		if(fsatisfaz(aux/10000.0000F))
		{
			return i;
		}
	}
	return -1;
}
int main()
{
	int T;
	int i, j;
	double m;
	int ini, fim, mid;
	cin >> T;
	while(T)
	{
		cin >> N >> F;
		F++;
		m = 1000000000;
		fini = ffim = 0.0000;
		for(i = 0; i < N; i++)
		{
			cin >> r[i];
			r[i] =* r[i]*PI;
			m = min(r[i], m);
		}
		m *= m*PI;
		if(!satisfaz(1))
		{
			cout << satisfaz2(0, 1);
		}
		else if(satisfaz(I(m)))
		{
			cout << satisfaz3(I(m), m);
		}
		else{
			ini = 1;
			fim = (int)m;
			while(ini < fim-1)
			{
				mid = (ini + fim)/2;
				if(satisfaz(mid))
				{
					ini = mid;
				}
				else
				{
					fim = mid;
				}
				cout << satisfaz2(ini, fim);
			}
			//o volume procurado esta entre ini e fim
		}
		T--;
	}
	return 0;
}