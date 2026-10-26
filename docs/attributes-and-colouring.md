# Tree annotations and attribute colouring

The development branch `codex/tree-annotations-and-attribute-colouring` extends
rhaeTree's existing annotated-tree model. It uses a shared attribute catalogue
for label fields, numeric shape sizes and colour mappings.

## Import and interpretation

- Newick internal names remain unchanged, including numeric names. Quoted names,
  doubled quotes, Unicode, and semicolons inside quoted names are preserved.
- BEAST/FigTree `[&key=value]` and NHX `[&&NHX:key=value:...]` annotations are
  imported as named fields. Ordinary comments are not treated as attributes.
- NEXUS `TAXLABELS` annotations are applied to matching tips after `TRANSLATE`.
  A tree's node-local annotation takes precedence over its taxon default.
- Scalars must be finite numbers. Mixed text is not partially parsed as a
  number. Two numeric values form an interval; longer lists remain lists and
  are not converted into uncertainty bars.
- Invalid trees produce an import error rather than silently disappearing from
  a multiple-tree file.

In **Appearance**, choose how to interpret internal labels for the current tree:

| Mode | Available derived fields |
| --- | --- |
| Auto | Internal support, or support 1 and 2 for `a/b` |
| Node names | Original names only |
| Single support value | Internal support |
| Two support values | Internal support 1 and 2, in the original order |
| Bootstrap | `bootstrap`, restricted to 0-100 |
| Posterior probability | `posterior`, restricted to 0-1 |
| FastTree local support | `local_support`, restricted to 0-1 |
| IQ-TREE SH-aLRT / UFBoot | `SH-aLRT` and `UFboot`, restricted to 0-100 |

Auto does not assign a statistical algorithm to an unnamed support value.
It treats unquoted numeric labels in 0-100 as generic support, keeps quoted
numeric names as names, and leaves other labels unchanged. Choose a source
mode explicitly when its meaning is known. For example, FastTree local support
is not a Bayesian posterior probability. The IQ-TREE profile applies specifically
to two-component SH-aLRT/UFBoot output, not every possible IQ-TREE test combination.
Explicit named annotations take precedence over a derived field with the same name.
Values retain their original scale; no implicit 0-1/0-100 normalization occurs.

The interpretation is saved separately for each tree in RTR. Virtual fields
such as `!support` are computed from the original label rather than written over
the original data.

## Labels, colours and edits

- **Labels: Tip**, **Labels: Node** and **Labels: Branch** offer attribute fields.
  Tip and internal-node catalogues contain only fields present in that scope.
  Internal-node options no longer depend on whether tips have names.
- Shape size selectors list scalar numeric fields in their own scope, including
  derived support fields.
- **Appearance** offers independent mappings for branches, tip labels, tip shapes
  and internal-node shapes. Choose Auto, Continuous or Categorical.
- Continuous maps support low/high colours, an automatic range in the current
  tree, a fixed range for comparing trees, and an explicit missing-value colour.
  Constant ranges use the middle colour. Invalid fixed ranges fall back to the
  current tree's range.
- Categorical colours are deterministic for the same text, including across trees.
  Different values can have similar colours; an automatic legend and editable
  per-category palette are not part of this change.
- Manual colour overrides and imported FigTree `!color` annotations take
  precedence over attribute mappings. Missing fields remain selected when
  switching trees, with a missing-field message and missing-value colour.
- Select a node or tip, open **Selected node: attributes** in Appearance, then
  set a named annotation. New attribute edits and interpretation/mapping changes
  support Undo/Redo. Reserved `!` fields are read-only in this editor.

Mapping choices are shared across tree navigation, while interpretation modes
belong to individual trees. Existing manual colour/highlight maps remain
node-ID-based; this change does not introduce per-tree manual styling.

All mappings feed the shared scene graph, so on-screen rendering, PNG, SVG and
PDF use the same resolution rules. RTR stores the mapping settings, additional
label fields, explicit annotations and each tree's interpretation.

Rerooting can change the biological meaning of clade annotations. This change
preserves node annotations and raw names through existing topology operations;
it does not infer whether an arbitrary annotation belongs to a node, clade or
undirected edge, and does not reassign statistical support between reversed edges.

## Reference designs

The implementation follows FigTree's ideas of context-sensitive attribute
selection and shared attribute-to-style mappings; its Java code is not copied.

- FigTree attribute selection:
  <https://github.com/rambaut/figtree/blob/master/src/figtree/treeviewer/painters/AttributeComboHelper.java>
- FigTree appearance mappings:
  <https://github.com/rambaut/figtree/blob/master/src/figtree/treeviewer/TreeAppearanceController.java>
- IQ-TREE SH-aLRT/UFBoot ordering:
  <https://iqtree.github.io/doc/Tutorial#assessing-branch-supports-with-single-branch-tests>

Open `examples/annotated-sources.nex` to try annotated BEAST-style, paired-support
and NHX trees in the same file.
