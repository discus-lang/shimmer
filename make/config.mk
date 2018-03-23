# Default build configuration.
#
#   This file is under version control.
#   If you want to override these options then create a file make/config-override.mk
#   and assign the appropdiate variables there.
#

# GHC Config ------------------------------------------------------------------
# GHC binary to use when building.
GHC  = ghc
GHCI = ghci
# with a cabal sandbox
# GHC    = cabal exec ghc --
# GHCI    = cabal exec ghci --

# Override default config with local config.
-include make/config-override.mk

