use super::{rectangular, Tree, TreeLayout, TreeLayoutType};

pub(super) fn build(tree: &Tree) -> Option<TreeLayout> {
    rectangular::build(tree).map(|mut layout| {
        layout.layout_type = TreeLayoutType::Phylogram;
        layout
    })
}
