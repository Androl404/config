local function map(mode, lhs, rhs, opts)
	local options = { noremap = true, silent = true }
	if opts then
		options = vim.tbl_extend("force", options, opts)
	end
	vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map("", "<up>", "<nop>")
map("", "<down>", "<nop>")
map("", "<left>", "<nop>")
map("", "<right>", "<nop>")

map("i", "<up>", "<nop>")
map("i", "<down>", "<nop>")
map("i", "<left>", "<nop>")
map("i", "<right>", "<nop>")

vim.opt.nu = true
vim.opt.rnu = true
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.smartindent = true
vim.opt.termguicolors = true

vim.keymap.set("n", "<C-e>", ":Neotree filesystem reveal current<CR>")
vim.keymap.set("n", "<C-c>", ":Compile ")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)

local plugins = {
	{
		"catppuccin/nvim",
		name = "catppuccin",
		priority = 1000,
		opts = {
			color_overrides = {
				mocha = {
					base = "#000000",
					mantle = "#000000",
					crust = "#000000",
				},
			},
		},
	},
	{ "rafi/awesome-vim-colorschemes" },
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.6",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	{ "nvim-treesitter/nvim-treesitter", build = ":TSUpdate" },
	{
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v3.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
			"MunifTanjim/nui.nvim",
		},
		use_libuv_file_watcher = true,
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons" },
	},
	{
		"williamboman/mason.nvim",
		"williamboman/mason-lspconfig.nvim",
		"neovim/nvim-lspconfig",
	},
	{ "nvim-telescope/telescope-ui-select.nvim" },
	{ "nvimtools/none-ls.nvim" },
	{ "hrsh7th/nvim-cmp" },
	{
		"L3MON4D3/LuaSnip",
		dependencies = {
			"saadparwaiz1/cmp_luasnip",
			"rafamadriz/friendly-snippets",
		},
	},
	{ "hrsh7th/cmp-nvim-lsp" },
	{
		"amitds1997/remote-nvim.nvim",
		version = "*", -- Pin to GitHub releases
		dependencies = {
			"nvim-lua/plenary.nvim", -- For standard functions
			"MunifTanjim/nui.nvim", -- To build the plugin UI
			"nvim-telescope/telescope.nvim", -- For picking b/w different remote methods
		},
		config = true,
	},
	{
		"windwp/nvim-autopairs",
		event = "InsertEnter",
		config = true,
		-- use opts = {} for passing setup options
		-- this is equalent to setup({}) function
	},
	{ "terrortylor/nvim-comment" },
	{
		"mg979/vim-visual-multi",
		lazy = true,
	},
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		opts = {},
	},
	{ "rcarriga/nvim-notify" },
	-- {
	--      "romgrk/barbar.nvim",
	--      dependencies = {
	--          "lewis6991/gitsigns.nvim", -- OPTIONAL: for git status
	--          "nvim-tree/nvim-web-devicons", -- OPTIONAL: for file icons
	--      },
	--      init = function()
	--          vim.g.barbar_auto_setup = false
	--      end,
	--      opts = {
	--          -- lazy.nvim will automatically call setup for you. put your options here, anything missing will use the default:
	--          animation = true,
	--          -- insert_at_start = false,
	--             auto_hide = 1,
	--             clickable = true,
	--             focus_on_close = 'previous',
	--          -- …etc.
	--      },
	--      -- version = "^1.0.0", -- optional: only update when a new 1.x version is released
	-- },
	{
		"tummetott/reticle.nvim",
		event = "VeryLazy", -- optionally lazy load the plugin
		opts = {
			-- add options here if you wish to override the default settings
		},
	},
	-- {
	--      "karb94/neoscroll.nvim",
	-- },
	{
		"ej-shafran/compile-mode.nvim",
		branch = "latest",
		-- or a specific version:
		-- tag = "v4.0.0"
		dependencies = {
			"nvim-lua/plenary.nvim",
			-- if you want to enable coloring of ANSI escape codes in
			-- compilation output, add:
			-- { "m00qek/baleia.nvim", tag = "v1.3.0" },
		},
		config = function()
			---@type CompileModeOpts
			vim.g.compile_mode = {
				-- to add ANSI escape code support, add:
				-- baleia_setup = true,
			}
		end,
	},
	{
		"lervag/vimtex",
		lazy = false, -- we don't want to lazy load VimTeX
		-- tag = "v2.15", -- uncomment to pin to a specific release
		init = function()
			-- VimTeX configuration goes here, e.g.
			vim.g.vimtex_view_method = "zathura"
		end,
	},
	{ "blazkowolf/gruber-darker.nvim" },
	{ "github/copilot.vim" },
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim", -- required
			"sindrets/diffview.nvim", -- optional - Diff integration

			-- Only one of these is needed.
			"nvim-telescope/telescope.nvim", -- optional
			"ibhagwan/fzf-lua", -- optional
			"echasnovski/mini.pick", -- optional
		},
		config = true,
	},
}

local opts = {}

require("lazy").setup(plugins, opts)
-- require("catppuccin").setup()

-- vim.cmd.colorscheme("catppuccin-mocha") -- catppuccin-latte, catppuccin-frappe, catppuccin-macchiato, catppuccin-mocha
vim.cmd.colorscheme("gruber-darker")

local builtin = require("telescope.builtin")
vim.keymap.set("n", "<C-p>", builtin.find_files, {})
vim.keymap.set("n", "fg", builtin.live_grep, {})
vim.keymap.set("n", "<C-n>", ":Neotree filesystem toggle left<CR>")

local configs = require("nvim-treesitter.configs")

-- require("nvim-treesitter.install").compilers = { "clang" }

configs.setup({
	ensure_installed = { "c", "cpp", "vim", "php", "lua", "html", "css", "javascript" },
	auto_install = true,
	highlight = { enable = true },
	indent = { enable = true },
})

require("lualine").setup({
	options = {
		theme = "auto",
		section_separators = "",
		component_separators = "",
	},
	sections = {
		lualine_a = { "mode" },
		lualine_b = { "branch", "diff", "diagnostics" },
		lualine_c = { {
			"filename",
			path = 1,
		} },
		lualine_x = { "encoding", "fileformat", "filetype" },
		lualine_y = { "progress" },
		lualine_z = { "location" },
	},
})

require("mason").setup()
require("mason-lspconfig").setup({
	ensure_installed = { "lua_ls", "clangd", "phpactor", "ltex", "texlab" },
})

local capabilities = require("cmp_nvim_lsp").default_capabilities()
local lspconfig = require("lspconfig")
lspconfig.lua_ls.setup({
	capabilities = capabilities,
})
lspconfig.ltex.setup({
	capabilities = capabilities,
	settings = {
		language = "fr_FR",
	},
})
lspconfig.texlab.setup({
	capabilities = capabilities,
})
lspconfig.clangd.setup({
	capabilities = capabilities,
})
-- lspconfig.phpactor.setup({
--     capabilities = capabilities,
-- })
lspconfig.intelephense.setup({
	capabilities = capabilities,
})
lspconfig.cssls.setup({
	capabilities = capabilities,
})
-- lspconfig.biome.setup({
--      capabilities = capabilities,
-- })
-- lspconfig.eslint.setup({
--   --- ...
--   on_attach = function(client, bufnr)
--     vim.api.nvim_create_autocmd("BufWritePre", {
--       buffer = bufnr,
--       command = "EslintFixAll",
--     })
--   end,
-- })
lspconfig.ts_ls.setup({})
lspconfig.emmet_language_server.setup({
	filetypes = {
		"css",
		"eruby",
		"html",
		"javascript",
		"javascriptreact",
		"less",
		"sass",
		"scss",
		"php",
		"pug",
		"typescriptreact",
	},
	-- Read more about this options in the [vscode docs](https://code.visualstudio.com/docs/editor/emmet#_emmet-configuration).
	-- **Note:** only the options listed in the table are supported.
	init_options = {
		---@type table<string, string>
		includeLanguages = {},
		--- @type string[]
		excludeLanguages = {},
		--- @type string[]
		extensionsPath = {},
		--- @type table<string, any> [Emmet Docs](https://docs.emmet.io/customization/preferences/)
		preferences = {},
		--- @type boolean Defaults to `true`
		showAbbreviationSuggestions = true,
		--- @type "always" | "never" Defaults to `"always"`
		showExpandedAbbreviation = "always",
		--- @type boolean Defaults to `false`
		showSuggestionsAsSnippets = false,
		--- @type table<string, any> [Emmet Docs](https://docs.emmet.io/customization/syntax-profiles/)
		syntaxProfiles = {},
		--- @type table<string, string> [Emmet Docs](https://docs.emmet.io/customization/snippets/#variables)
		variables = {},
	},
})
capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
lspconfig.html.setup({
	capabilities = capabilities,
})
require("lspconfig").html.setup({})

vim.keymap.set("n", "K", vim.lsp.buf.hover, {})
vim.keymap.set("n", "gd", vim.lsp.buf.definition, {})
vim.keymap.set({ "n" }, "ca", vim.lsp.buf.code_action, {})

require("telescope").setup({
	extensions = {
		["ui-select"] = {
			require("telescope.themes").get_dropdown({
				-- even more opts
			}),

			-- pseudo code / specification for writing custom displays, like the one
			-- for "codeactions"
			-- specific_opts = {
			--   [kind] = {
			--     make_indexed = function(items) -> indexed_items, width,
			--     make_displayer = function(widths) -> displayer
			--     make_display = function(displayer) -> function(e)

			--     make_ordinal = function(e) -> string
			--   },
			--   -- for example to disable the custom builtin "codeactions" display
			--      do the following
			--   codeactions = false,
			-- }
		},
	},
})
-- To get ui-select loaded and working with telescope, you need to call
-- load_extension, somewhere after setup function:
require("telescope").load_extension("ui-select")

local null_ls = require("null-ls")
null_ls.setup({
	sources = {
		null_ls.builtins.formatting.stylua,
		-- null_ls.builtins.formatting.latexindent,
	},
})
vim.keymap.set("n", "gf", vim.lsp.buf.format, {})

local cmp = require("cmp")
require("luasnip.loaders.from_vscode").lazy_load()

cmp.setup({
	snippet = {
		-- REQUIRED - you must specify a snippet engine
		expand = function(args)
			-- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
			require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
			-- require('snippy').expand_snippet(args.body) -- For `snippy` users.
			-- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
			-- vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
		end,
	},
	window = {
		-- completion = cmp.config.window.bordered(),
		-- documentation = cmp.config.window.bordered(),
	},
	mapping = cmp.mapping.preset.insert({
		["<C-b>"] = cmp.mapping.scroll_docs(-4),
		["<C-f>"] = cmp.mapping.scroll_docs(4),
		["<C-Space>"] = cmp.mapping.complete(),
		["<C-e>"] = cmp.mapping.abort(),
		["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
	}),
	sources = cmp.config.sources({
		{ name = "nvim_lsp" },
		-- { name = "vsnip" }, -- For vsnip users.
		{ name = "luasnip" }, -- For luasnip users.
		-- { name = 'ultisnips' }, -- For ultisnips users.
		-- { name = 'snippy' }, -- For snippy users.
	}, {
		{ name = "buffer" },
	}),
})

require("nvim_comment").setup({
	comment_empty = false,
})

require("ibl").setup()
-- require'lspconfig'.eslint.setup{}

vim.notify = require("notify")

require("reticle").setup({
	always_highlight_number = true,
})

vim.api.nvim_set_hl(0, "CursorLine", { bg = "#262626" })

-- require("neoscroll").setup({
--      mappings = { -- Keys to be mapped to their corresponding default scrolling animation
--          "<C-u>",
--          "<C-d>",
--          "<C-b>",
--          "<C-f>",
--          "<C-y>",
--          "<C-e>",
--          "zt",
--          "zz",
--          "zb",
--      },
--      hide_cursor = true, -- Hide cursor while scrolling
--      stop_eof = true, -- Stop at <EOF> when scrolling downwards
--      respect_scrolloff = false, -- Stop scrolling when the cursor reaches the scrolloff margin of the file
--      cursor_scrolls_alone = true, -- The cursor will keep on scrolling even if the window cannot scroll further
--      easing = "linear", -- Default easing function
--      pre_hook = nil, -- Function to run before the scrolling animation starts
--      post_hook = nil, -- Function to run after the scrolling animation ends
--      performance_mode = false, -- Disable "Performance Mode" on all buffers.
-- })
