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

unordered_map<char, pair<size_t, size_t>>
keys_reachable(const vector<char> &m,
               size_t w, size_t h,
               pair<size_t, size_t> start)
{
  unordered_map<char, pair<size_t, size_t>> ret;
  unordered_map<pair<size_t, size_t>, bool, hash_pair> visited;
  queue<pair<size_t, size_t>> q;

  q.push(start);

  auto inrange = [w, h](size_t x, size_t y) -> bool {
    return x < w && y < h;
  };
  auto wall = [&m = as_const(m), w](size_t x, size_t y) -> bool
  {
    return m[y * w + x] == '#';
  };

  while (q.size())
  {
    size_t x, y;
    pair<size_t, size_t> pos = q.front();
    tie(x, y) = pos;

    visited[pos] = true;

    char c = m[y * w + x];
    if (c >= 'a' && c <= 'z')
    {
      ret[c] = pos;
    }

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
        q.push(npos);
      }
      dirs.pop();
    }

    q.pop();
  }

  return ret;
}

unordered_map<char, pair<keys, size_t>>
bfs(const vector<char> &m,
    size_t w, size_t h,
    const unordered_map<char, pair<size_t, size_t>> &km,
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
    if (it.first != '@')
    {
      ret[it.first] = keys_req[it.second];
    }
  }

  return ret;
}

extern "C"
void
solve(int _)
{
  vector<string> v;
  vector<char> m;
  size_t w = 0, h = 0;
  vector<unordered_map<char, pair<size_t, size_t>>> km;
  unordered_map<pair<char, char>, pair<keys, size_t>, hash_pair> key_dists;
  vector<pair<size_t, size_t>> starts;

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

  if (_)
  {
    bool done = false;
    for (size_t y = 0; y < h && !done; y++)
    {
      for (size_t x = 0; x < w && !done; x++)
      {
        if (v[y][x] == '@')
        {
          v[y - 1][x] = '#';
          v[y + 1][x] = '#';
          v[y][x - 1] = '#';
          v[y][x + 1] = '#';
          v[y][x]     = '#';
          v[y - 1][x - 1] = '@';
          v[y + 1][x - 1] = '@';
          v[y - 1][x + 1] = '@';
          v[y + 1][x + 1] = '@';
          done = true;
        }
      }
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
        starts.push_back(make_pair(x, y));
      }
    }
  }

  km.resize(starts.size());
  for (size_t i = 0; i < starts.size(); i++)
  {
    km[i] = keys_reachable(m, w, h, starts[i]);
    unordered_map<char, pair<keys, size_t>> keydata = bfs(m, w, h, km[i], starts[i]);
    for (auto it : keydata)
    {
      key_dists[make_pair('@', it.first)] = it.second;
    }
    for (auto it : km[i])
    {
      unordered_map<char, pair<keys, size_t>> keydata = bfs(m, w, h, km[i], it.second);
      for (auto reach : keydata)
      {
        key_dists[make_pair(it.first, reach.first)] = reach.second;
      }
    }
  }

  unordered_map<pair<keys, string>, bool, hash_pair> visited;
  unordered_map<pair<keys, string>, size_t, hash_pair> dists;
  auto cmp = [&d = dists](pair<keys, string> l, pair<keys, string> r) -> bool
  {
    return d[l] > d[r];
  };

  // initially seed the priority queue
  priority_queue<pair<keys, string>, vector<pair<keys, string>>, decltype(cmp)> pq(cmp);
  // vector<pair<keys, string>> pq;
  unordered_map<pair<keys, string>, bool, hash_pair> pqi;

  string initial;
  for (size_t i = 0; i < starts.size(); i++)
  {
    initial += "@";
  }
  pq.push(make_pair(keys(), initial));
  pqi[make_pair(keys(), initial)] = true;
  // make_heap(pq.begin(), pq.end(), cmp);

  keys end;
  for (size_t i = 0; i < starts.size(); i++)
  {
    for (auto it : km[i])
    {
      end.set(to_keybit(it.first));
    }
  }
  // cout << "end: " << end << endl;

  size_t cnt = 0;
  size_t min = -1;
  while (pq.size()) {
    keys ks;
    string lasts;
    pair<keys, string> kl;

    // pop_heap(pq.begin(), pq.end(), cmp);
    kl = pq.top();
    tie(ks, lasts) = kl;
    pq.pop();
    pqi[kl] = false;

    if (visited[kl]) continue;
    visited[kl] = true;

    if (ks == end)
    {
      if (dists[kl] < min) min = dists[kl];
    }

    //if (cnt % 1000 == 0)
    //{
    //  string s;
    //  for (auto c : lasts) s += c;
    //  // cout << "trying keyset " << ks << " at " << s << " (" << pq.size() << ")" << endl;
    //}
    //cnt++;

    for (size_t i = 0; i < lasts.size(); i++)
    {
      // try to move the ith robot to a key reachable by it
      for (auto it : km[i])
      {
        char c = it.first;
        keys req, missing;
        size_t ndist;
        tie(req, ndist) = key_dists[make_pair(lasts[i], c)];
        missing = req & ~ks;

        if (missing.none())
        {
          keys nks = ks;
          string nlasts = lasts;
          nks.set(to_keybit(c));
          nlasts[i] = c;
          pair<keys, string> nk = make_pair(nks, nlasts);
          size_t new_total = dists[kl] + ndist;

          if (dists[nk] == 0)
          {
            dists[nk] = new_total;
          }
          else
          {
            dists[nk] = dists[nk] <= new_total ? dists[nk] : new_total;
          }
          if (!pqi[nk])
          {
            pq.push(nk);
            pqi[nk] = true;
          }
          // cout << last << " -> " << c << " (req, curdist, dist): ("
          //      << req << ", " << dists[kl] << ", " << ndist << ")" << endl;
        }
      }
    }
  }

  cout << min << endl;
}
