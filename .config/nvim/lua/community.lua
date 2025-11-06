-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  -- { import = "astrocommunity.colorscheme.catppuccin" },
  { import = "astrocommunity.colorscheme.nordic-nvim" },
  -- { import = "astrocommunity.colorscheme.nord-nvim" },
  -- { import = "astrocommunity.colorscheme.github-nvim-theme" },
  -- { import = "astrocommunity.colorscheme.kanagawa-nvim" },
  -- { import = "astrocommunity.colorscheme.tokyonight-nvim" },

  { import = "astrocommunity.pack.lua" },

  { import = "astrocommunity.pack.cpp" },
  { import = "astrocommunity.pack.cmake" },

  -- { import = "astrocommunity.pack.go" },
  -- { import = "astrocommunity.pack.templ" },

  { import = "astrocommunity.pack.bash" },
  -- { import = "astrocommunity.pack.julia" },
  -- { import = "astrocommunity.pack.java" },
  -- { import = "astrocommunity.pack.rust" },
  { import = "astrocommunity.pack.zig" },
  { import = "astrocommunity.pack.python" },

  -- { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.pack.html-css" },
  { import = "astrocommunity.pack.tailwindcss" },
  --
  { import = "astrocommunity.pack.json" },
  { import = "astrocommunity.pack.yaml" },
  { import = "astrocommunity.pack.markdown" },

  -- { import = "astrocommunity.pack.terraform" },
  { import = "astrocommunity.git.fugit2-nvim" },

  { import = "astrocommunity.diagnostics.trouble-nvim" },
  -- Inlay hints only in visual mode
  { import = "astrocommunity.recipes.astrolsp-no-insert-inlay-hints" },

  --  Synchronize Vim, Tmux, and OS clipboards via OSC 52
  -- { import = "astrocommunity.terminal-integration.vim-tmux-yank" },
}
