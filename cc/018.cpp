#include <cassert>
#include <bitset>
#include <iostream>
#include <map>
#include <queue>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

using namespace std;

#define N 26

typedef bitset<N> keys;

class hash_pair {
public:
  template <class T1, class T2>
  size_t operator()(const pair<T1, T2>& p) const
  {
    return hash<T1>{}(p.first) ^ hash<T2>{}(p.second);
  }
};

size_t
to_keybit(char c)
{
  if (c >= 'a' && c <= 'z')
  {
    return c - 'a';
  }
  else
  {
    assert(false);
  }
  exit(5);
}

// pair<keys needed, distance>
unordered_map<char, pair<keys, size_t>>
bfs(vector<char> m,
    size_t w, size_t h,
    unordered_map<char, pair<size_t, size_t>> km,
    pair<size_t, size_t> last)
{
  unordered_map<pair<size_t, size_t>, pair<keys, size_t>, hash_pair> keys_req;
  unordered_map<pair<size_t, size_t>, bool, hash_pair> visited;
  queue<tuple<pair<size_t, size_t>, keys, size_t>> q;
  unordered_map<char, pair<keys, size_t>> ret;

  auto inrange = [w, h](size_t x, size_t y) -> bool {
    return x < w && y < h;
  };
  auto wall = [&m = as_const(m), w](size_t x, size_t y) -> bool
  {
    return m[y * w + x] == '#';
  };

  q.push(make_tuple(last, keys(), 0));

  while (q.size())
  {
    pair<size_t, size_t> pos;
    keys ks;
    size_t dist;
    size_t x, y;
    tie(pos, ks, dist) = q.front();
    tie(x, y) = pos;

    keys_req[pos] = make_pair(ks, dist);
    visited[pos] = true;

    queue<pair<size_t, size_t>> dirs;
    dirs.push(make_pair(x - 1, y));
    dirs.push(make_pair(x + 1, y));
    dirs.push(make_pair(x, y - 1));
    dirs.push(make_pair(x, y + 1));

    while (dirs.size())
    {
      pair<size_t, size_t> npos;
      size_t xx, yy;
      npos = dirs.front();
      tie(xx, yy) = npos;

      if (inrange(xx, yy) && !visited[npos] && !wall(xx, yy))
      {
        keys nks = ks;
        char c = m[yy * w + xx];

        if (c >= 'A' && c <= 'Z')
        {
          nks.set(c - 'A');
        }

        q.push(make_tuple(npos, nks, dist + 1));
      }

      dirs.pop();
    }

    q.pop();
  }

  for (auto it : km)
  {
    ret[it.first] = keys_req[it.second];
  }

  return ret;
}

extern "C"
void
solve(int _)
{
  vector<string> v;
  vector<char> m;
  unordered_map<char, pair<size_t, size_t>> km;
  unordered_map<char, unordered_map<char, pair<keys, size_t>>> key_dists;
  size_t w = 0;
  size_t h = 0;
  pair<size_t, size_t> init;

  while (!cin.eof())
  {
    string s;
    cin >> s;
    if (s != "")
    {
      if (w == 0) w = s.length();
      else assert(w == s.length());
      v.push_back(s);
      h++;
    }
  }

  m.resize(w * h);
  for (size_t y = 0; y < h; y++)
  {
    for (size_t x = 0; x < w; x++)
    {
      m[y * w + x] = v[y][x];
      if (v[y][x] == '@')
      {
        init = make_pair(x, y);
      }
      else if (v[y][x] >= 'a' && v[y][x] <= 'z')
      {
        km[v[y][x]] = make_pair(x, y);
      }
    }
  }

  for (auto it : km)
  {
    key_dists[it.first] = bfs(m, w, h, km, it.second);
  }

  unordered_map<pair<keys, char>, bool, hash_pair> visited;
  unordered_map<pair<keys, char>, size_t, hash_pair> dists;
  auto cmp = [&d = dists](pair<keys, char> l, pair<keys, char> r) -> bool
  {
    return d[l] > d[r];
  };

  // initially seed the priority queue
  priority_queue<pair<keys, char>, vector<pair<keys, char>>, decltype(cmp)> pq(cmp);
  unordered_map<char, pair<keys, size_t>> initial = bfs(m, w, h, km, init);
  for (auto it : km)
  {
    char c = it.first;
    if (initial[c].first.none())
    {
      pair<keys, char> k = make_pair(keys().set(to_keybit(c)), c);
      dists[k] = initial[c].second;
      pq.push(k);

      // cout << "(req: " << initial[c].first << ", final: " << c << ", dist: " << dists[k] << ")" << endl;
    }
  }

  while (pq.size()) {
    keys ks;
    char last;
    pair<keys, char> kl = pq.top();
    tie(ks, last) = kl;
    pq.pop();

    if (visited[kl]) continue;
    visited[kl] = true;

    for (auto it : km)
    {
      char c = it.first;
      keys req, missing;
      size_t ndist;
      tie(req, ndist) = key_dists[last][c];
      missing = req & ~ks;

      if (missing.none())
      {
        keys nks = ks;
        nks.set(to_keybit(c));
        pair<keys, char> nk = make_pair(nks, c);
        size_t new_total = dists[kl] + ndist;

        if (dists[nk] == 0)
        {
          dists[nk] = new_total;
        }
        else
        {
          dists[nk] = dists[nk] <= new_total ? dists[nk] : new_total;
        }
        pq.push(nk);
        // cout << last << " -> " << c << " (req, curdist, dist): ("
        //      << req << ", " << dists[kl] << ", " << ndist << ")" << endl;
      }
    }
  }

  keys end;
  for (auto it : km)
  {
    end.set(to_keybit(it.first));
  }

  // cout << "end: " << end << endl;

  size_t min = -1;
  for (auto it : km)
  {
    size_t d = dists[make_pair(end, it.first)];
    if (d < min) min = d;
  }

  cout << min << endl;
}
