use super::*;
use crate::tree::attributes::{AttributeKind, AttributeScope, InternalLabelMode};
use crate::tree::colouring::{ColourMapping, ColourMode};
use crate::tree::layout::{TreeLayout, TreeLayoutType};
use crate::tree::painter::{BranchLabelDisplay, NodeLabelDisplay, TipLabelDisplay, TreePainter};
use crate::tree::scene_graph::ScenePrimitive;
use crate::tree::viewer::TreeViewer;
use eframe::egui::Color32;

fn tree(raw: &str) -> Tree {
    parse_newick(raw).unwrap().remove(0)
}

#[test]
fn preserves_quoted_names_comments_and_annotation_delimiters() {
    let t = tree(
        r#"('A; B'[ordinary prose][&host="A&B, C",note="x]y"]:1,B[&&NHX:S=human:B=97]:2)'100';"#,
    );
    let a = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("A; B"))
        .unwrap();
    assert_eq!(a.get_attribute("host").map(String::as_str), Some("A&B, C"));
    assert_eq!(a.get_attribute("note").map(String::as_str), Some("x]y"));
    assert!(!a.attributes.contains_key("ordinary prose"));
    let b = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("B"))
        .unwrap();
    assert_eq!(b.get_attribute("S").map(String::as_str), Some("human"));
    assert_eq!(b.get_numeric_attribute("B"), Some(97.0));
    let root = &t.nodes[t.root.unwrap()];
    assert_eq!(root.name.as_deref(), Some("100"));
    assert!(root.name_quoted);
    assert!(t.attribute_value(root, "!support").is_none());
}

#[test]
fn quoted_names_with_doubled_quotes_and_unicode_survive() {
    let t = tree("('O''Brien; sample':1,\"A \"\"quoted\"\" taxon\":2,'样本':3);");
    let names: Vec<_> = t
        .external_nodes()
        .iter()
        .map(|n| n.name.as_deref().unwrap())
        .collect();
    assert_eq!(names, ["O'Brien; sample", "A \"quoted\" taxon", "样本"]);
}

#[test]
fn plain_comments_and_mixed_text_are_not_numeric_annotations() {
    let t = tree("(A[plain text][&mixed=\"host 123\",nan=NaN,infinite=inf,range={0.1,0.9},partial={0.1,unknown},vector={0.1,0.2,0.7}]:1,B:1); [trailing comment]");
    let a = t
        .external_nodes()
        .into_iter()
        .find(|n| n.name.as_deref() == Some("A"))
        .unwrap();
    for key in ["mixed", "nan", "infinite", "partial", "vector"] {
        assert!(a.get_numeric_attribute(key).is_none(), "{key}");
        assert!(a.numeric_range_attribute(key).is_none(), "{key}");
    }
    assert_eq!(a.numeric_range_attribute("range"), Some((0.1, 0.9)));
}

#[test]
fn ordinary_comment_punctuation_does_not_break_statement_scanning() {
    let t = tree("[Author's tree; [nested comment]] (A[don't infer attributes]:1,B:1);");
    assert_eq!(t.leaf_count(), 2);
    let trees =
        parse_nexus("#NEXUS\n[Author's tree; [nested]] begin trees; tree a=(A:1,B:1); end;")
            .unwrap();
    assert_eq!(trees.len(), 1);
}

#[test]
fn distinguishes_sources_without_guessing_algorithm_names() {
    let mut t = tree("((A:1,B:1)95/100:1,(C:1,D:1)0.98:1)Root;");
    let pair = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("95/100"))
        .unwrap()
        .id;
    let scalar = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("0.98"))
        .unwrap()
        .id;
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[pair], "!support.1"),
        Some(95.0)
    );
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[pair], "!support.2"),
        Some(100.0)
    );
    assert!(t.attribute_value(&t.nodes[pair], "UFboot").is_none());
    assert!(t.attribute_value(&t.nodes[scalar], "posterior").is_none());
    t.internal_label_mode = InternalLabelMode::IqTreeAlrtUfboot;
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[pair], "SH-aLRT"),
        Some(95.0)
    );
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[pair], "UFboot"),
        Some(100.0)
    );
    t.internal_label_mode = InternalLabelMode::Posterior;
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[scalar], "posterior"),
        Some(0.98)
    );
    t.internal_label_mode = InternalLabelMode::FastTreeLocal;
    assert!(t.attribute_value(&t.nodes[scalar], "posterior").is_none());
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[scalar], "local_support"),
        Some(0.98)
    );
    t.internal_label_mode = InternalLabelMode::Names;
    assert!(t.attribute_value(&t.nodes[pair], "!support.1").is_none());
    assert_eq!(t.nodes[pair].name.as_deref(), Some("95/100"));
}

#[test]
fn explicit_named_attributes_override_interpretations_and_modes_validate_units() {
    let mut t = tree("((A:1,B:1)95[&posterior=0.97,bootstrap=88]:1,C:1);");
    let id = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("95"))
        .unwrap()
        .id;
    t.internal_label_mode = InternalLabelMode::Bootstrap;
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[id], "bootstrap"),
        Some(88.0)
    );
    t.internal_label_mode = InternalLabelMode::Posterior;
    assert!(t.attribute_value(&t.nodes[id], "!support").is_none());
    assert_eq!(
        t.numeric_attribute_value(&t.nodes[id], "posterior"),
        Some(0.97)
    );
}

#[test]
fn catalogue_is_scoped_and_detects_mixed_types() {
    let t = tree("((A[&host=human,value=1]:1,B[&host=bird,value=unknown]:1)[&posterior=0.99,hpd={1,2}]:1,C:1);");
    let tips = t.attribute_catalogue(AttributeScope::Tips);
    assert!(!tips.iter().any(|a| a.key == "posterior"));
    let mixed = tips.iter().find(|a| a.key == "value").unwrap();
    assert_eq!(mixed.kind, AttributeKind::Mixed);
    assert_eq!((mixed.present, mixed.total), (2, 3));
    let internal = t.attribute_catalogue(AttributeScope::Internal);
    assert!(!internal.iter().any(|a| a.key == "host"));
    assert_eq!(
        internal.iter().find(|a| a.key == "hpd").unwrap().kind,
        AttributeKind::Range
    );
}

#[test]
fn nexus_taxon_annotations_translate_and_local_precedence() {
    let raw = r#"#NEXUS
begin taxa; dimensions ntax=2; taxlabels
 'A taxon'[&host="human & bird",!color=#e8291c] B[&host=bird];
end;
begin trees; translate 1 'A taxon', 2 B;
tree first = [&R] (1[&host=local]:1,2:1);
tree second = [&R] (1:1,2:1); end;"#;
    let trees = parse_nexus(raw).unwrap();
    assert_eq!(trees.len(), 2);
    let tip = |index: usize| {
        trees[index]
            .nodes
            .iter()
            .find(|n| n.name.as_deref() == Some("A taxon"))
            .unwrap()
    };
    assert_eq!(
        tip(0).get_attribute("host").map(String::as_str),
        Some("local")
    );
    assert_eq!(
        tip(1).get_attribute("host").map(String::as_str),
        Some("human & bird")
    );
    assert_eq!(
        TreePainter::annotation_color(tip(0)),
        Some(Color32::from_rgb(232, 41, 28))
    );
}

#[test]
fn malformed_trees_are_reported_instead_of_silently_skipped() {
    assert!(parse_nexus(
        "#NEXUS\nbegin trees; tree good = (A:1,B:1); tree bad = (A:bad,B:1); end;"
    )
    .is_err());
    assert!(parse_newick("(A:1,B:1); (C:1,D:1)").is_err());
    assert!(parse_newick("(A[&note=\"unterminated]:1,B:1);").is_err());
    assert!(parse_nexus("#NEXUS\nbegin taxa; taxlabels =bad; end;").is_err());
}

#[test]
fn compact_nexus_commands_and_quoted_tree_names_are_supported() {
    let trees = parse_nexus("#NEXUS\nbegin trees; tree\t'a=b'=(A:1,B:1); end;").unwrap();
    assert_eq!(trees.len(), 1);
    assert_eq!(trees[0].label.as_deref(), Some("a=b"));
}

#[test]
fn colour_mapping_handles_missing_values_constant_ranges_and_category_stability() {
    let t = tree("(A[&score=0,host=human]:1,B[&score=100,host=bird]:1,C:1);");
    let mut mapping = ColourMapping {
        attribute: Some("score".into()),
        ..Default::default()
    };
    let colors = mapping.resolve(&t, AttributeScope::Tips);
    let tips = t.external_nodes();
    assert_eq!(colors[tips[0].id], Some(mapping.low));
    assert_eq!(colors[tips[1].id], Some(mapping.high));
    assert_eq!(colors[tips[2].id], Some(mapping.missing));
    mapping.range = Some((0.0, 0.0));
    assert!(
        mapping
            .resolve(&t, AttributeScope::Tips)
            .iter()
            .flatten()
            .count()
            == 3
    );
    mapping.attribute = Some("host".into());
    mapping.mode = ColourMode::Categorical;
    let first = mapping.resolve(&t, AttributeScope::Tips)[tips[0].id];
    let other = tree("(X[&host=fish]:1,A[&host=human]:1);");
    let id = other
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("A"))
        .unwrap()
        .id;
    assert_eq!(first, mapping.resolve(&other, AttributeScope::Tips)[id]);
}

#[test]
fn rtr_round_trips_per_tree_interpretations_mapping_and_original_labels() {
    let mut trees = parse_newick("((A:1,B:1)95/100:1,C:1); ((D:1,E:1)'100':1,F:1);").unwrap();
    trees[0].internal_label_mode = InternalLabelMode::IqTreeAlrtUfboot;
    trees[1].internal_label_mode = InternalLabelMode::Names;
    let mapping = ColourMapping {
        attribute: Some("UFboot".into()),
        range: Some((80.0, 100.0)),
        ..Default::default()
    };
    let mut settings = BTreeMap::new();
    mapping.write_settings("colour.branch", &mut settings);
    let path = tests::temp_rtr_path("attribute-mapping");
    save_rtr(&path, &trees, &settings).unwrap();
    let loaded = load_trees(&path).unwrap();
    fs::remove_file(&path).unwrap();
    assert_eq!(
        loaded.trees[0].internal_label_mode,
        InternalLabelMode::IqTreeAlrtUfboot
    );
    assert_eq!(
        loaded.trees[1].internal_label_mode,
        InternalLabelMode::Names
    );
    assert!(loaded.trees[1]
        .nodes
        .iter()
        .any(|n| n.name.as_deref() == Some("100") && n.name_quoted));
    let mut restored = ColourMapping::default();
    restored.read_settings("colour.branch", &loaded.metadata);
    assert_eq!(restored, mapping);
}

#[test]
fn attribute_edits_and_interpretation_survive_reroot_and_order_restore() {
    let t = tree("(((A:1,B:1)95:1,C:1)90:1,D:1);");
    let id = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("95"))
        .unwrap()
        .id;
    let tip = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("C"))
        .unwrap()
        .id;
    let mut viewer = TreeViewer::new();
    viewer.set_trees(vec![t]);
    viewer.set_internal_label_mode(InternalLabelMode::Bootstrap);
    viewer.set_node_attribute(id, "location", "China");
    viewer.reroot_at_branch(tip);
    viewer.apply_node_ordering(true);
    viewer.set_node_attribute(id, "host", "human");
    viewer.restore_unordered_tree();
    let t = viewer.current_tree().unwrap();
    assert_eq!(t.internal_label_mode, InternalLabelMode::Bootstrap);
    assert_eq!(
        t.nodes[id].get_attribute("location").map(String::as_str),
        Some("China")
    );
    assert_eq!(
        t.nodes[id].get_attribute("host").map(String::as_str),
        Some("human")
    );
}

#[test]
fn restoring_root_snapshot_keeps_later_annotation_edits() {
    let t = tree("((A:1,B:1)95:1,C:1);");
    let id = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("95"))
        .unwrap()
        .id;
    let mut viewer = TreeViewer::new();
    viewer.set_trees(vec![t]);
    let mut snapshot = viewer.snapshot_current_tree().unwrap();
    viewer.midpoint_root();
    viewer.set_internal_label_mode(InternalLabelMode::Bootstrap);
    viewer.set_node_attribute(id, "location", "Asia");
    snapshot.sync_annotations(viewer.current_tree_index(), viewer.current_tree().unwrap());
    viewer.restore_snapshot(snapshot);
    let t = viewer.current_tree().unwrap();
    assert_eq!(t.internal_label_mode, InternalLabelMode::Bootstrap);
    assert_eq!(
        t.nodes[id].get_attribute("location").map(String::as_str),
        Some("Asia")
    );
}

#[test]
fn source_examples_keep_expected_scientific_fields() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("examples");
    for name in [
        "FigTree.tre",
        "HA_continuous_MCC.tre",
        "HA_discrete_MCC.tre",
        "OsLDP.fasttree.nex",
        "annotated-sources.nex",
    ] {
        let bundle = load_trees(&root.join(name)).unwrap();
        assert!(!bundle.trees.is_empty(), "{name}");
        assert!(
            !bundle.trees[0]
                .attribute_catalogue(AttributeScope::All)
                .is_empty(),
            "{name}"
        );
    }
    let bundle = load_trees(&root.join("OsLDP.fasttree.nex")).unwrap();
    assert!(bundle.trees[0]
        .nodes
        .iter()
        .any(|node| TreePainter::annotation_color(node).is_some()));
}

#[test]
fn labels_and_manual_colour_overrides_are_shared_by_scene_and_all_exporters() {
    let t = tree("((A[&host=human,score=0]:1,B[&host=bird,score=100]:1)95:1,C:1);");
    let mut painter = TreePainter::default();
    painter.show_node_labels = true;
    painter.node_label_display = NodeLabelDisplay::Attribute;
    painter.node_label_attribute = Some("!support".into());
    painter.tip_label_display = TipLabelDisplay::Attribute;
    painter.tip_label_attribute = Some("host".into());
    painter.branch_label_display = BranchLabelDisplay::Attribute;
    painter.branch_label_attribute = Some("score".into());
    painter.branch_colour_mapping.attribute = Some("score".into());
    let id = t
        .nodes
        .iter()
        .find(|n| n.name.as_deref() == Some("B"))
        .unwrap()
        .id;
    let manual = Color32::from_rgb(17, 34, 51);
    painter.set_branch_color(id, manual);
    let layout = TreeLayout::from_tree(&t, TreeLayoutType::Rectangular).unwrap();
    let scene = crate::export::svg::build_export_scene(&t, &layout, &painter, 640.0, 480.0);
    assert!(scene
        .primitives
        .iter()
        .any(|p| matches!(p, ScenePrimitive::Text { text, .. } if text == "human")));
    assert!(scene
        .primitives
        .iter()
        .any(|p| matches!(p, ScenePrimitive::Text { text, .. } if text == "95.00")));
    assert!(scene.primitives.iter().any(
        |p| matches!(p, ScenePrimitive::StrokePolyline { style, .. } if style.color == manual)
    ));
    let path = tests::temp_rtr_path("coloured-export");
    let svg = path.with_extension("svg");
    let pdf = path.with_extension("pdf");
    let png = path.with_extension("png");
    crate::export::svg::export_svg(&t, &layout, &painter, &svg, 640.0, 480.0).unwrap();
    crate::export::pdf::export_pdf(&t, &layout, &painter, &pdf, 640.0, 480.0).unwrap();
    let image = crate::tree::skia_renderer::SkiaTreeRenderer::new()
        .render_scene_to_image(&scene, 1.0, 1.0)
        .unwrap()
        .image;
    let pixels: Vec<_> = image
        .pixels
        .iter()
        .flat_map(Color32::to_srgba_unmultiplied)
        .collect();
    image::save_buffer(&png, &pixels, 640, 480, image::ColorType::Rgba8).unwrap();
    assert!(fs::read_to_string(&svg).unwrap().contains("#112233"));
    assert!(fs::read(&pdf).unwrap().starts_with(b"%PDF"));
    assert_eq!(image::image_dimensions(&png).unwrap(), (640, 480));
    for path in [svg, pdf, png] {
        fs::remove_file(path).unwrap();
    }
}
