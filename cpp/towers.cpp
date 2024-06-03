#include <chrono>
#include <initializer_list>
#include <iostream>
#include <tuple>
#include <utility>

#include "z3++.h"

struct puzzle {
  std::initializer_list<std::tuple<int, int, int>> cell;
  std::initializer_list<std::pair<int, int>> top;
  std::initializer_list<std::pair<int, int>> left;
  std::initializer_list<std::pair<int, int>> bottom;
  std::initializer_list<std::pair<int, int>> right;
};

z3::expr satVis(z3::context& ctx, int n, z3::expr_vector l) {
  // From https://github.com/RichardBradley/z3-skyscraper-solver
  // This method is faster for small number of edge conditions.
  int s = int(l.size());
  if (n < 1 || n > s) return ctx.bool_val(false);
  if (l.size() == 1) return ctx.bool_val(n == 1);
  if (n == 1) {
    z3::expr_vector v(ctx);
    for (int i = 1; i < s; i++) v.push_back(l[i] < l[0]);
    return z3::mk_and(v);
  }
  z3::expr_vector opts(ctx);
  for (int i = 1; i <= 1 + s - n; i++) {
    z3::expr_vector v(ctx);
    for (int j = 1; j < i; j++) v.push_back(l[0] > l[j]);
    v.push_back(l[0] < l[i]);
    z3::expr_vector rem(ctx);
    for (int j = i; j < s; j++) rem.push_back(l[j]);
    v.push_back(satVis(ctx, n - 1, rem));
    opts.push_back(z3::mk_and(v));
  }
  return z3::mk_or(opts);
}

bool solve(int size, puzzle p, bool useSatVis) {
  z3::context ctx;
  z3::expr n = ctx.int_val(size);
  z3::sort rs = ctx.int_sort();  // Restrict from 1 to n
  z3::func_decl grid = ctx.function("grid", rs, rs, rs);
  z3::func_decl top = ctx.function("top", rs, rs);
  z3::func_decl bottom = ctx.function("bottom", rs, rs);
  z3::func_decl left = ctx.function("left", rs, rs);
  z3::func_decl right = ctx.function("right", rs, rs);
  auto visible = [](z3::expr_vector l) {
    z3::expr m = l[0];
    z3::expr r = m.ctx().int_val(1);
    for (int i = 1; i < int(l.size()); i++) {
      r = r + z3::ite(l[i] > m, m.ctx().int_val(1), m.ctx().int_val(0));
      m = z3::ite(l[i] > m, l[i], m);
    }
    return r;
  };
  auto findVis = [](int i, std::initializer_list<std::pair<int, int>> l) {
    for (auto [j, v] : l) {
      if (i == j) return v;
    }
    return 0;
  };

  z3::solver s(ctx);
  z3::expr x = ctx.int_const("x");
  z3::expr y = ctx.int_const("y");
  s.add(z3::forall(x, y, grid(x, y) <= n && grid(x, y) > 0));
  for (int i = 1; i <= size; i++) {
    z3::expr_vector row(ctx), col(ctx), rrow(ctx), rcol(ctx);
    for (int j = 1; j <= size; j++) {
      row.push_back(grid(ctx.int_val(i), ctx.int_val(j)));
      col.push_back(grid(ctx.int_val(j), ctx.int_val(i)));
      rrow.push_back(grid(ctx.int_val(i), ctx.int_val(size + 1 - j)));
      rcol.push_back(grid(ctx.int_val(size + 1 - j), ctx.int_val(i)));
    }
    s.add(z3::distinct(row));
    s.add(z3::distinct(col));
    if (!useSatVis) {
      s.add(visible(row) == left(ctx.int_val(i)));
      s.add(visible(col) == top(ctx.int_val(i)));
      s.add(visible(rrow) == right(ctx.int_val(i)));
      s.add(visible(rcol) == bottom(ctx.int_val(i)));
    } else {
      if (int v = findVis(i, p.left); v) s.add(satVis(ctx, v, row));
      if (int v = findVis(i, p.top); v) s.add(satVis(ctx, v, col));
      if (int v = findVis(i, p.right); v) s.add(satVis(ctx, v, rrow));
      if (int v = findVis(i, p.bottom); v) s.add(satVis(ctx, v, rcol));
    }
  }
  for (auto [i, j, val] : p.cell) {
    s.add(grid(ctx.int_val(i), ctx.int_val(j)) == ctx.int_val(val));
  }
  if (!useSatVis) {
    auto vis = [&](z3::func_decl dir,
                   std::initializer_list<std::pair<int, int>> v) {
      for (auto [i, val] : v) {
        s.add(dir(ctx.int_val(i)) == ctx.int_val(val));
      }
    };
    vis(top, p.top);
    vis(left, p.left);
    vis(bottom, p.bottom);
    vis(right, p.right);
  }

  std::cout << "solver: " << s << "\n";

  switch (auto res = s.check(); res) {
    case z3::sat: {
      auto m = s.get_model();
      for (int i = 1; i <= size; i++) {
        for (int j = 1; j <= size; j++) {
          std::cout << m.eval(grid(ctx.int_val(i), ctx.int_val(j))) << " ";
        }
        std::cout << "\n";
      }
      return true;
    }
    default:
      std::cout << "no solution found: " << res << "\n";
      return false;
  }
}

bool puzzles(int size, bool useSatVis) {
  puzzle p;
  if (size == 5) {
    p = puzzle{
        .cell = {{2, 2, 1}},
        .top = {{1, 2}},
        .left = {{3, 2}},
        .bottom = {{3, 1}, {4, 3}},
        .right = {{3, 2}, {4, 2}, {5, 3}},
    };
  } else if (size == 7) {
    p = puzzle{
        .cell =
            {{2, 1, 3}, {3, 1, 1}, {3, 3, 2}, {6, 5, 3}, {7, 1, 2}, {7, 6, 3}},
        .top = {{2, 3}, {4, 3}, {7, 4}},
        .left = {{2, 3}, {4, 1}, {6, 3}},
        .bottom = {{1, 3}, {3, 1}, {4, 3}, {5, 3}},
        .right = {{2, 3}, {3, 3}, {5, 3}, {7, 4}},
    };
  } else if (size == 8) {
    p = puzzle{
        .cell = {{1, 3, 5},
                 {2, 6, 3},
                 {3, 3, 2},
                 {3, 8, 1},
                 {4, 5, 5},
                 {4, 8, 4},
                 {5, 7, 1},
                 {6, 4, 1},
                 {6, 6, 2},
                 {8, 2, 3}},
        .top = {{1, 3}, {4, 4}, {5, 4}, {6, 4}},
        .left = {{2, 4}, {5, 4}, {6, 3}, {7, 3}},
        .bottom = {{1, 1}, {2, 3}, {6, 3}, {8, 4}},
        .right = {{1, 3}, {4, 4}, {7, 3}},
    };
  } else if (size == 9) {
    p = puzzle{
        .cell = {{1, 2, 7},
                 {3, 1, 3},
                 {3, 5, 5},
                 {3, 8, 4},
                 {4, 3, 2},
                 {5, 6, 2},
                 {6, 2, 2},
                 {6, 3, 1},
                 {6, 5, 3},
                 {7, 2, 3},
                 {7, 8, 7},
                 {8, 6, 6}},
        .top = {{3, 3}, {4, 2}, {5, 2}, {6, 3}, {7, 3}, {8, 2}},
        .left = {{3, 3}, {4, 3}, {5, 3}, {6, 2}, {7, 4}, {8, 3}, {9, 4}},
        .bottom = {{1, 4}, {2, 4}, {4, 2}, {5, 2}, {7, 3}, {8, 4}, {9, 4}},
        .right = {{2, 3}, {3, 2}},
    };
  }
  return solve(size, p, useSatVis);
}

int main(int argc, char* argv[]) {
  const auto start = std::chrono::high_resolution_clock::now();
  puzzles(argc > 1 ? std::atoi(argv[1]) : 7, true);
  const auto end = std::chrono::high_resolution_clock::now();
  std::cout << "Time: " << std::chrono::duration<double>(end - start).count()
            << "s\n";
}