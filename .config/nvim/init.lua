-- load all plugins
require "pluginsList.lua"
require "file-icons.lua"

require "misc-utils.lua"
require "bufferline.lua"
require "statusline.lua"

require("colorizer").setup()
require("neoscroll").setup() -- smooth scroll

-- lsp
require "lspconfig.lua"
require "compe.lua"

local cmd = vim.cmd
local g = vim.g

g.auto_save = 0


-- colorscheme related stuff

cmd "syntax enable"
cmd "syntax on"

local base16 = require "base16"
base16(base16.themes["onedark"], true)

-- blankline

local indent = 2

g.indentLine_enabled = 1
g.indent_blankline_char = "▏"

cmd "hi IndentBlanklineChar guifg=#2a2e36"
cmd "set guifont=FiraCode:h10"
cmd "set nowritebackup"
cmd "set noswapfile"
cmd "set smartcase"
cmd "set noerrorbells"
cmd "hi NvimTreeVertSplit guifg=#1e222a"


g.indent_blankline_filetype_exclude = {"help", "terminal"}
g.indent_blankline_buftype_exclude = {"terminal"}

g.indent_blankline_show_trailing_blankline_indent = false
g.indent_blankline_show_first_indent_level = false

g.vista_disable_statusline = true
g.vista_default_executive = 'ctags'
g.vista_echo_cursor_strategy = 'floating_win'
g.vista_vimwiki_executive = 'markdown'

require "treesitter.lua"
require "mappings.lua"

-- highlights --
cmd "hi LineNr guifg=#42464e guibg=NONE"
cmd "hi Comment guifg=#42464e"

cmd "hi SignColumn guibg=NONE"
cmd "hi VertSplit guibg=NONE guifg=#2a2e36"
cmd "hi EndOfBuffer guifg=#1e222a"
cmd "hi PmenuSel guibg=#98c379"
cmd "hi Pmenu  guibg=#282c34"

cmd "hi Normal guibg=NONE ctermbg=NONE"

require "telescope.lua"
require "nvimTree.lua"

-- git signs
require "gitsigns.lua"
require("nvim-autopairs").setup()

require("lspkind").init(
    {
        with_text = true,
        symbol_map = {
            Folder = ""
        }
    }
)

-- Dashboard

--g.dashboard_default_executive="telescope"
--g.dashboard_custom_shortcut_icon['last_session'] = ' '
--g.dashboard_custom_shortcut_icon['find_history'] = 'ﭯ '
--g.dashboard_custom_shortcut_icon['find_file'] = ' '
--g.dashboard_custom_shortcut_icon['new_file'] = ' '
--g.dashboard_custom_shortcut_icon['change_colorscheme'] = ' '
--g.dashboard_custom_shortcut_icon['find_word'] = ' '
--g.dashboard_custom_shortcut_icon['book_marks'] = ' '

-- hide line numbers in terminal windows
vim.api.nvim_exec([[
   au BufEnter term://* setlocal nonumber
]], false)

-- inactive statuslines as thin splitlines
cmd("highlight! StatusLineNC gui=underline guibg=NONE guifg=#383c44")

cmd "hi clear CursorLine"
cmd "hi cursorlinenr guibg=NONE guifg=#abb2bf"
cmd "hi normal ctermbg=NONE"
-- setup for TrueZen.nvim
require "zenmode.lua"
