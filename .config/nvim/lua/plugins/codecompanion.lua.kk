return {
  "olimorris/codecompanion.nvim",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "nvim-lua/plenary.nvim",
  },
  cmd = {
    "CodeCompanion",
    "CodeCompanionChat",
    "CodeCompanionToggle",
    "CodeCompanionActions",
  },
  config = function()
    require("codecompanion").setup {
      adapters = {
        chat = require("codecompanion.adapters").extend("ollama", { schema = { model = { default = "phi4" } } }),
        inline = require("codecompanion.adapters").extend("ollama", { schema = { model = { default = "phi4" } } }),
      },
    }
  end,
}
