#include <cassert>
#include <iostream>
#include <map>
#include <queue>
#include <string>
#include <vector>

using namespace std;

typedef struct tile_t
{
  bool wall;
  string portal;
} tile_t;

size_t
bfs(vector<char> m,
    size_t w, size_t h,
    pair<size_t, size_t> start, pair<size_t, size_t> end,
    map<pair<char, char>, vector<pair<size_t, size_t>>> p,
    size_t depth)
{
  map<pair<pair<size_t, size_t>, size_t>, bool> visited;
  map<pair<pair<size_t, size_t>, size_t>, bool> queued;
  map<pair<pair<size_t, size_t>, size_t>, size_t> dists;
  queue<pair<pair<size_t, size_t>, size_t>> q;

  auto inrange = [w, h](size_t x, size_t y) -> bool {
    return x < w && y < h;
  };

  dists[make_pair(start, 0)] = 0;
  q.push(make_pair(start, 0));
  while (q.size())
  {
    size_t x, y, _;
    pair<pair<size_t, size_t>, size_t> posdepth;
    pair<size_t, size_t> pos;
    queue<pair<size_t, size_t>> dirs;
    posdepth = q.front();
    tie(pos, _) = posdepth;
    tie(x, y) = pos;

    if (pos == end && _ == 0)
    {
      return dists[posdepth];
    }
    visited[posdepth] = true;

    dirs.push(make_pair(-1, 0));
    dirs.push(make_pair(1, 0));
    dirs.push(make_pair(0, -1));
    dirs.push(make_pair(0, 1));

    while (dirs.size())
    {
      size_t dx, dy;
      pair<size_t, size_t> p1;
      tie(dx, dy) = dirs.front();

      p1 = make_pair(x + dx, y + dy);
      if (inrange(x + dx, y + dy))
      {
        char c = m[(y + dy) * w + x + dx];
        if (c == '.')
        {
          pair<pair<size_t, size_t>, size_t> npos = make_pair(p1, _);
          if (!visited[npos] && !queued[npos])
          {
            dists[npos] = dists[posdepth] + 1;
            queued[npos] = true;
            q.push(npos);
          }
        }
        else if (isalpha(c))
        {
          size_t edgex = x + dx + dx, edgey = y + dy + dy;
          pair<size_t, size_t> p2 = make_pair(edgex, edgey);
          char d = m[edgey * w + edgex];
          bool inner;
          size_t delta;
          pair<char, char> portal = p1 <= p2 ? make_pair(c, d) : make_pair(d, c);

          if (edgex == 0 || edgex == w - 1 ||
              edgey == 0 || edgey == h - 1)
          {
            inner = false;
          }
          else
          {
            inner = true;
          }
          delta = inner ? depth : -depth;
          for (auto npos : p[portal])
          {
            pair<pair<size_t, size_t>, size_t> nposd = make_pair(npos, _ + delta);
            if (npos == pos) continue;
            if (delta == -1 && _ == 0) continue;
            if (!visited[nposd] && !queued[nposd])
            {
              dists[nposd] = dists[posdepth] + 1;
              queued[nposd] = true;
              q.push(nposd);
            }
          }
        }
      }

      dirs.pop();
    }

    q.pop();
  }

  assert(false);
  exit(5);
}

extern "C"
void
solve(int _)
{
  vector<string> v;
  vector<char> m;
  map<pair<char, char>, vector<pair<size_t, size_t>>> p;
  size_t w = 0, h = 0;
  pair<size_t, size_t> aa, zz;
  size_t depth = _ ? 1 : 0;

  while (!cin.eof())
  {
    string s;
    getline(cin, s);
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
      char c = v[y][x];
      m[y * w + x] = c;
      if (c == '.')
      {
        queue<pair<pair<size_t, size_t>, pair<size_t, size_t>>> stuff;
        stuff.push(make_pair(make_pair(x - 2, y), make_pair(x - 1, y)));
        stuff.push(make_pair(make_pair(x + 1, y), make_pair(x + 2, y)));
        stuff.push(make_pair(make_pair(x, y - 2), make_pair(x, y - 1)));
        stuff.push(make_pair(make_pair(x, y + 1), make_pair(x, y + 2)));

        while (stuff.size())
        {
          size_t x1, y1, x2, y2;
          pair<size_t, size_t> p1, p2;
          tie(p1, p2) = stuff.front();
          tie(x1, y1) = p1;
          tie(x2, y2) = p2;
          if (isalpha(v[y1][x1]) &&
              isalpha(v[y2][x2]))
          {
            p[make_pair(v[y1][x1], v[y2][x2])].push_back(make_pair(x, y));
          }
          stuff.pop();
        }
      }
    }
  }

  aa = p[make_pair('A', 'A')][0];
  zz = p[make_pair('Z', 'Z')][0];

  cout << bfs(m, w, h, aa, zz, p, depth) << endl;
}
