PACKAGE_NAME=malt
SOURCE_DIR="."

BUILD_FLAGS=
TEST_FLAGS=-q

#----------------------------------------
# Sources. For clarity, we list all the source
# files explicitly included in the make. If you
# add sources be sure to update this section.
#----------------------------------------

FLAT_DIR=flat-tensors
NESTED_DIR=nested-tensors
TOOLS_DIR=tools
MALTED_DIR=malted

# flat-tensors
FLAT_TENSORS_DIR=$(FLAT_DIR)/tensors
FLAT_AUTODIFF_DIR=$(FLAT_DIR)/autodiff
FLAT_EXT_OPS_DIR=$(FLAT_DIR)/ext-ops

FLAT_TENSORS_SOURCES=\
  $(FLAT_TENSORS_DIR)/0-vectors.rkt\
  $(FLAT_TENSORS_DIR)/1-flats.rkt\
  $(FLAT_TENSORS_DIR)/A-equality.rkt\
  $(FLAT_TENSORS_DIR)/B-tensor-basics.rkt\
  $(FLAT_TENSORS_DIR)/C-tensor-ops.rkt\
  $(FLAT_TENSORS_DIR)/D-extend.rkt\
  $(FLAT_DIR)/tensors.rkt

FLAT_AUTODIFF_SOURCES=\
  $(FLAT_AUTODIFF_DIR)/A-autodiff.rkt\
  $(FLAT_AUTODIFF_DIR)/B-prims.rkt\
  $(FLAT_DIR)/autodiff.rkt

FLAT_EXT_OPS_SOURCES=\
  $(FLAT_EXT_OPS_DIR)/A-scalar-ops.rkt\
  $(FLAT_EXT_OPS_DIR)/B-comparators.rkt\
  $(FLAT_EXT_OPS_DIR)/C-star-2-1.rkt\
  $(FLAT_EXT_OPS_DIR)/D-sum.rkt\
  $(FLAT_EXT_OPS_DIR)/E-argmax.rkt\
  $(FLAT_EXT_OPS_DIR)/F-max.rkt\
  $(FLAT_EXT_OPS_DIR)/G-correlate.rkt\
  $(FLAT_DIR)/ext-ops.rkt

FLAT_LOADERS=\
  $(FLAT_DIR)/no-duals-no-overrides.rkt\
  $(FLAT_DIR)/no-duals.rkt\
  $(FLAT_DIR)/no-overrides.rkt\
  $(FLAT_DIR)/tensors.rkt\
  $(FLAT_DIR)/autodiff.rkt\
  $(FLAT_DIR)/ext-ops.rkt

FLAT_SOURCES=$(FLAT_TENSORS_SOURCES)\
	$(FLAT_AUTODIFF_SOURCES)\
	$(FLAT_EXT_OPS_SOURCES)\
	$(FLAT_LOADERS)

# nested-tensors
NESTED_TENSORS_DIR=$(NESTED_DIR)/tensors
NESTED_AUTODIFF_DIR=$(NESTED_DIR)/autodiff
NESTED_EXT_OPS_DIR=$(NESTED_DIR)/ext-ops

NESTED_TENSORS_SOURCES=\
  $(NESTED_TENSORS_DIR)/A-equality.rkt\
  $(NESTED_TENSORS_DIR)/B-tensor-basics.rkt\
  $(NESTED_TENSORS_DIR)/C-tensor-ops.rkt\
  $(NESTED_TENSORS_DIR)/D-extend.rkt\
  $(NESTED_DIR)/tensors.rkt

NESTED_AUTODIFF_SOURCES=\
  $(NESTED_AUTODIFF_DIR)/A-autodiff.rkt\
  $(NESTED_AUTODIFF_DIR)/B-prims.rkt\
  $(NESTED_DIR)/autodiff.rkt

NESTED_EXT_OPS_SOURCES=\
  $(NESTED_EXT_OPS_DIR)/A-scalar-ops.rkt\
  $(NESTED_EXT_OPS_DIR)/B-comparators.rkt\
  $(NESTED_EXT_OPS_DIR)/C-star-2-1.rkt\
  $(NESTED_EXT_OPS_DIR)/D-sum.rkt\
  $(NESTED_EXT_OPS_DIR)/E-argmax.rkt\
  $(NESTED_EXT_OPS_DIR)/F-max.rkt\
  $(NESTED_EXT_OPS_DIR)/G-correlate.rkt\
  $(NESTED_DIR)/ext-ops.rkt

NESTED_LOADERS=\
  $(NESTED_DIR)/no-duals-no-overrides.rkt\
  $(NESTED_DIR)/no-duals.rkt\
  $(NESTED_DIR)/no-overrides.rkt\
  $(NESTED_DIR)/tensors.rkt\
  $(NESTED_DIR)/autodiff.rkt\
  $(NESTED_DIR)/ext-ops.rkt

NESTED_SOURCES=$(NESTED_TENSORS_SOURCES)\
  $(NESTED_AUTODIFF_SOURCES)\
  $(NESTED_EXT_OPS_SOURCES)\
  $(NESTED_LOADERS)

# tools
TOOLS_SOURCES=\
  $(TOOLS_DIR)/A-hypers.rkt\
  $(TOOLS_DIR)/B-random.rkt\
  $(TOOLS_DIR)/C-logging.rkt\
  tools.rkt

# malted
MALTED_SOURCES=\
  $(MALTED_DIR)/A-core.rkt\
  $(MALTED_DIR)/B-layer-fns.rkt\
  $(MALTED_DIR)/C-loss.rkt\
  $(MALTED_DIR)/D-gradient-descent.rkt\
  $(MALTED_DIR)/E-gd-common.rkt\
  $(MALTED_DIR)/F-naked.rkt\
  $(MALTED_DIR)/G-velocity.rkt\
  $(MALTED_DIR)/H-rms.rkt\
  $(MALTED_DIR)/I-adam.rkt\
  $(MALTED_DIR)/J-stochastic.rkt\
  $(MALTED_DIR)/K-dense.rkt\
  $(MALTED_DIR)/L-accuracy.rkt\
  $(MALTED_DIR)/M-recu.rkt\
  $(MALTED_DIR)/N-blocks.rkt\
  $(MALTED_DIR)/O-init.rkt\
  malted.rkt

# All the sources together, plus entry points
SOURCES=$(FLAT_SOURCES)\
  $(NESTED_SOURCES)\
  $(TOOLS_SOURCES)\
  $(MALTED_SOURCES)\
  impl.rkt\
  impl-no-duals.rkt\
  base.rkt\
  base-no-duals.rkt

# Files applicable to print.
TO_PRINT=$(SOURCES)

# Where raco is installed
RACO="$(shell which raco)"

# Build it all
all: build test

build: $(SOURCES)
	@ echo "Building ..." &&\
	  $(RACO) make $(BUILD_FLAGS) $(SOURCES)

# Test it all
test:
	@ echo "Running tests ..." &&\
	$(RACO) test $(TEST_FLAGS) $(SOURCES)

clean:
	find . -name 'compiled' -exec rm -rf {} \;

print:
	lpr $(TO_PRINT)

install:
	$(RACO) pkg install

uninstall:
	$(RACO) pkg remove $(PACKAGE_NAME)

reinstall: uninstall install

testOnly: $(SOURCES)
	$(RACO) make $(SOURCES)
	$(RACO) test `find . -name "$(module).rkt"`
