PACKAGE_NAME=malt
SOURCE_DIR="."

BUILD_FLAGS=
TEST_FLAGS=-q

#----------------------------------------
# Sources. For clarity, we list all the source
# files explicitly included in the make. If you
# add sources be sure to update this section.
#----------------------------------------

LAZY_DIR=lazy
LEARNER_DIR=learner
FLAT_DIR=flat-tensors
NESTED_DIR=nested-tensors
TOOLS_DIR=tools
MALTED_DIR=malted

# lazy
LAZY_TENSORS_DIR=$(LAZY_DIR)/tensors
LAZY_AUTODIFF_DIR=$(LAZY_DIR)/autodiff
LAZY_EXT_OPS_DIR=$(LAZY_DIR)/ext-ops

LAZY_TENSORS_SOURCES=\
  $(LAZY_TENSORS_DIR)/0-lazy.rkt\
  $(LAZY_TENSORS_DIR)/A-equality.rkt\
  $(LAZY_DIR)/tensors.rkt

LAZY_AUTODIFF_SOURCES=\
  $(LAZY_AUTODIFF_DIR)/A-autodiff.rkt\
  $(LAZY_AUTODIFF_DIR)/B-prims.rkt\
  $(LAZY_AUTODIFF_DIR)/C-dualized-tensor-ops.rkt\
  $(LAZY_AUTODIFF_DIR)/D-test-helpers.rkt\
  $(LAZY_DIR)/autodiff.rkt

LAZY_EXT_OPS_SOURCES=\
  $(LAZY_EXT_OPS_DIR)/A-scalar-ops.rkt\
  $(LAZY_EXT_OPS_DIR)/B-comparators.rkt\
  $(LAZY_EXT_OPS_DIR)/C-star-2-1.rkt\
  $(LAZY_EXT_OPS_DIR)/D-sum.rkt\
  $(LAZY_EXT_OPS_DIR)/E-argmax.rkt\
  $(LAZY_EXT_OPS_DIR)/F-max.rkt\
  $(LAZY_EXT_OPS_DIR)/G-correlate.rkt\
  $(LAZY_DIR)/ext-ops.rkt

LAZY_LOADERS=\
  $(LAZY_DIR)/no-duals-no-overrides.rkt\
  $(LAZY_DIR)/no-duals.rkt\
  $(LAZY_DIR)/no-overrides.rkt\
  $(LAZY_DIR)/tensors.rkt\
  $(LAZY_DIR)/autodiff.rkt\
  $(LAZY_DIR)/ext-ops.rkt

LAZY_SOURCES=$(LAZY_TENSORS_SOURCES)\
	$(LAZY_AUTODIFF_SOURCES)\
	$(LAZY_EXT_OPS_SOURCES)\
	$(LAZY_LOADERS)

# learner
LEARNER_TENSORS_DIR=$(LEARNER_DIR)/tensors
LEARNER_AUTODIFF_DIR=$(LEARNER_DIR)/autodiff
LEARNER_EXT_OPS_DIR=$(LEARNER_DIR)/ext-ops

LEARNER_TENSORS_SOURCES=\
  $(LEARNER_TENSORS_DIR)/0-duals.rkt\
  $(LEARNER_TENSORS_DIR)/B-tensor-basics.rkt\
  $(LEARNER_TENSORS_DIR)/C-tensor-ops.rkt\
  $(LEARNER_TENSORS_DIR)/D-extend.rkt\
  $(LEARNER_DIR)/tensors.rkt

LEARNER_AUTODIFF_SOURCES=\
  $(LEARNER_AUTODIFF_DIR)/A-autodiff.rkt\
  $(LEARNER_AUTODIFF_DIR)/B-prims.rkt\
  $(LEARNER_AUTODIFF_DIR)/D-test-helpers.rkt\
  $(LEARNER_AUTODIFF_DIR)/E-print.rkt\
  $(LEARNER_DIR)/autodiff.rkt

LEARNER_EXT_OPS_SOURCES=\
  $(LEARNER_EXT_OPS_DIR)/A-scalar-ops.rkt\
  $(LEARNER_EXT_OPS_DIR)/B-comparators.rkt\
  $(LEARNER_EXT_OPS_DIR)/C-star-2-1.rkt\
  $(LEARNER_EXT_OPS_DIR)/D-sum.rkt\
  $(LEARNER_EXT_OPS_DIR)/E-argmax.rkt\
  $(LEARNER_EXT_OPS_DIR)/F-max.rkt\
  $(LEARNER_EXT_OPS_DIR)/G-correlate.rkt\
  $(LEARNER_EXT_OPS_DIR)/H-rectify.rkt\
  $(LEARNER_EXT_OPS_DIR)/I-flatten.rkt\
  $(LEARNER_EXT_OPS_DIR)/J-nd-ops.rkt\
  $(LEARNER_EXT_OPS_DIR)/K-concat.rkt\
  $(LEARNER_DIR)/ext-ops.rkt

LEARNER_LOADERS=\
  $(LEARNER_DIR)/no-duals-no-overrides.rkt\
  $(LEARNER_DIR)/no-duals.rkt\
  $(LEARNER_DIR)/no-overrides.rkt\
  $(LEARNER_DIR)/tensors.rkt\
  $(LEARNER_DIR)/autodiff.rkt\
  $(LEARNER_DIR)/ext-ops.rkt

LEARNER_SOURCES=$(LEARNER_TENSORS_SOURCES)\
	$(LEARNER_AUTODIFF_SOURCES)\
	$(LEARNER_EXT_OPS_SOURCES)\
	$(LEARNER_LOADERS)

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
  $(FLAT_AUTODIFF_DIR)/C-dualized-tensor-ops.rkt\
  $(FLAT_AUTODIFF_DIR)/D-test-helpers.rkt\
  $(FLAT_AUTODIFF_DIR)/E-print.rkt\
  $(FLAT_DIR)/autodiff.rkt

FLAT_EXT_OPS_SOURCES=\
  $(FLAT_EXT_OPS_DIR)/A-scalar-ops.rkt\
  $(FLAT_EXT_OPS_DIR)/B-comparators.rkt\
  $(FLAT_EXT_OPS_DIR)/C-star-2-1.rkt\
  $(FLAT_EXT_OPS_DIR)/D-sum.rkt\
  $(FLAT_EXT_OPS_DIR)/E-argmax.rkt\
  $(FLAT_EXT_OPS_DIR)/F-max.rkt\
  $(FLAT_EXT_OPS_DIR)/G-correlate.rkt\
  $(FLAT_EXT_OPS_DIR)/I-flatten.rkt\
  $(FLAT_EXT_OPS_DIR)/K-concat.rkt\
  $(FLAT_DIR)/ext-ops.rkt

FLAT_LOADERS=\
  $(FLAT_DIR)/no-duals-no-overrides.rkt\
  $(FLAT_DIR)/no-duals.rkt\
  $(FLAT_DIR)/no-overrides.rkt\
  $(FLAT_DIR)/tensors.rkt\
  $(FLAT_DIR)/autodiff.rkt\
  $(FLAT_DIR)/ext-impl.rkt\
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
  $(NESTED_AUTODIFF_DIR)/C-dualized-tensor-ops.rkt\
  $(NESTED_AUTODIFF_DIR)/D-test-helpers.rkt\
  $(NESTED_AUTODIFF_DIR)/E-print.rkt\
  $(NESTED_DIR)/autodiff.rkt

NESTED_EXT_OPS_SOURCES=\
  $(NESTED_EXT_OPS_DIR)/A-scalar-ops.rkt\
  $(NESTED_EXT_OPS_DIR)/B-comparators.rkt\
  $(NESTED_EXT_OPS_DIR)/C-star-2-1.rkt\
  $(NESTED_EXT_OPS_DIR)/D-sum.rkt\
  $(NESTED_EXT_OPS_DIR)/E-argmax.rkt\
  $(NESTED_EXT_OPS_DIR)/F-max.rkt\
  $(NESTED_EXT_OPS_DIR)/G-correlate.rkt\
  $(NESTED_EXT_OPS_DIR)/I-flatten.rkt\
  $(NESTED_EXT_OPS_DIR)/K-concat.rkt\
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
SOURCES=$(LEARNER_SOURCES)\
  $(LAZY_SOURCES)\
  $(FLAT_SOURCES)\
  $(NESTED_SOURCES)\
  $(TOOLS_SOURCES)\
  $(MALTED_SOURCES)\
  ports.rkt\
  impl.rkt\
  impl-no-duals.rkt\
  impl-no-overrides.rkt\
  impl-no-duals-no-overrides.rkt\
  base.rkt\
  base-no-duals.rkt\
  base-no-overrides.rkt\
  base-no-duals-no-overrides.rkt\
  interlude-V.rkt\
  appendix-B.rkt\
  main.rkt

# Files applicable to print.
TO_PRINT=$(SOURCES)

# Documentation files
DOC_DIR=scribblings/docs
DOC_ROOT=scribblings/malt.scrbl
DOC_FILES=\
  scribblings/malt.scrbl\
  $(DOC_DIR)/entry-points.scrbl\
  $(DOC_DIR)/list-functions.scrbl\
  $(DOC_DIR)/tensor-functions.scrbl\
  $(DOC_DIR)/extension-functions.scrbl\
  $(DOC_DIR)/autodiff-functions.scrbl\
  $(DOC_DIR)/differentiable-operators.scrbl\
  $(DOC_DIR)/non-differentiable-operators.scrbl\
  $(DOC_DIR)/base-rank-operators.scrbl\
  $(DOC_DIR)/boolean-comparators.scrbl\
  $(DOC_DIR)/tensorized-comparators.scrbl\
  $(DOC_DIR)/hypers.scrbl\
  $(DOC_DIR)/loss.scrbl\
  $(DOC_DIR)/layer.scrbl\
  $(DOC_DIR)/gd.scrbl\
  $(DOC_DIR)/blocks.scrbl\
  $(DOC_DIR)/init.scrbl\
  $(DOC_DIR)/randoms.scrbl\
  $(DOC_DIR)/misc.scrbl\
  $(DOC_DIR)/logging.scrbl

# Where raco is installed
RACO="$(shell which raco)"
RACKET="$(shell which racket)"

# Optional ARG when provided
ARG=$(filter-out $@,$(MAKECMDGOALS))

# Build it all
all: build test

build: $(SOURCES)
	@ echo "Building ..." &&\
	  $(RACO) make $(BUILD_FLAGS) $(SOURCES)

# Test it all
test:
	@ echo "Running tests ..." &&\
	$(RACO) test $(TEST_FLAGS) $(SOURCES)

one:
	-@ $(RACO) make $(ARG) && $(RACO) test $(ARG)

clean:
	find . -name 'compiled' | xargs -I% rm -rf %

set-learner:
	$(RACKET) -e "(require malt/set-impl) (set-impl 'learner)"

set-nested-tensors:
	$(RACKET) -e "(require malt/set-impl) (set-impl 'nested-tensors)"

set-flat-tensors:
	$(RACKET) -e "(require malt/set-impl) (set-impl 'flat-tensors)"

doc: $(DOC_FILES)
	mkdir -p html
	raco scribble --quiet +m --htmls --dest html $(DOC_ROOT)

doc-pdf: $(DOC_FILES)
	raco scribble --quiet +m --pdf $(DOC_ROOT)

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

# Catch-all to avoid attempting making the ARG
%:
	-@
