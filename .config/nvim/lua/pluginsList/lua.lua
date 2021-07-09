-- check if packer is installed (~/local/share/nvim/site/pack)
local packer_exists = pcall(vim.cmd, [[packadd packer.nvim]])

-- using { } when using a different branch of the plugin or loading the plugin with certain commands
return require("packer").startup(
    function()
        use {"wbthomason/packer.nvim", opt = true}
        use {"lukas-reineke/indent-blankline.nvim", branch = "lua"}

        -- color related stuff
        use "norcalli/nvim-base16.lua"
        use "norcalli/nvim-colorizer.lua"

        -- lsp stuff
        use "nvim-treesitter/nvim-treesitter"
        use "neovim/nvim-lspconfig"
        use "hrsh7th/nvim-compe"
        use "onsails/lspkind-nvim"
        use "sbdchd/neoformat"
        use "nvim-lua/plenary.nvim"

        use "lewis6991/gitsigns.nvim"
        use "akinsho/nvim-bufferline.lua"
        use "glepnir/galaxyline.nvim"
        use "windwp/nvim-autopairs"
        use "alvan/vim-closetag"

        -- file managing , picker etc
        use "kyazdani42/nvim-tree.lua"
        use "kyazdani42/nvim-web-devicons"
        use "ryanoasis/vim-devicons"
        use "nvim-telescope/telescope.nvim"
        use "nvim-telescope/telescope-media-files.nvim"
        use "nvim-lua/popup.nvim"
        use 'iamcco/markdown-preview.nvim'

        -- misc
        use "tweekmonster/startuptime.vim"
        use "urbainvaes/vim-tmux-pilot"
        use "907th/vim-auto-save"
        use "kdav5758/TrueZen.nvim"
        use "karb94/neoscroll.nvim"
        use "tomasiser/vim-code-dark"
        use 'maksimr/vim-jsbeautify'
        use 'roy2220/easyjump.tmux'
        use 'tpope/vim-repeat'
        use "sheerun/vim-polyglot"
        use 'folke/lsp-colors.nvim'
        use 'liuchengxu/vista.vim'
        use {
          "folke/trouble.nvim",
           requires = "kyazdani42/nvim-web-devicons",
           config = function()
           require("trouble").setup {
      -- your configuration comes here
      -- or leave it empty to use the default settings
      -- refer to the configuration section below
           }
         end
        }
        --use "tiagofumo/vim-nerdtree-syntax-highlight"
        --use "PhilRunninger/nerdtree-visual-selection"
        --use "Xuyuanp/nerdtree-git-plugin"         
    end
)
