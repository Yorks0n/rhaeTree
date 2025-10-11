# Turning an SVG into a PDF in rust

2023 January 17 · Emily Selwood

I have some code that creates pages of shapes. My wife uses this to create products for her [etsy shop](https://www.etsy.com/uk/shop/FaerydaeStitches). I originally wrote this in java many years ago. Some time last year I decided to rewrite the entire thing in rust. There were a number of reasons for this. Mostly that I’d learned a heck of a lot since I designed the original architecture and wanted some features that would not be possible with out rewriting most of it any way.

So the rust rewrite happened. Everything was wonderful and lovely. Ish. I did have to build my own SVG classes to do what I needed. Thankfully the [quick_xml](https://docs.rs/quick-xml/latest/quick_xml/) library made this reasonably easy. While I don’t mind creating readers and writers for SVG files, PDFs are another kettle of fish.

In java land I used the `batik-transcoder` which worked wonderfully. In rust I started out using the aptly named [svg2pdf](https://docs.rs/svg2pdf/latest/svg2pdf/) library and thought I was done.

Unfortunately I eventually discovered, when we went to upload a file of pdfs to etsy that was too big, that the svg2pdf library uses [usvg](https://docs.rs/usvg/latest/usvg/) under the hood. This is a wonderful library that simplifies the svg down as much as possible to make later operations have to deal with the smallest subset of svg as possible.

One of the main things it does is convert everything to a path object. This does remove the need for a lot of special handling, circle? its a path, rect? its a path, text? its a path. Wait what? Yeah, it converts every single character of text into a path object. The watermark we put in the background to stop people reselling our files suddenly made the files 5x bigger.

I know PDFs can handle this, the original java version was handling text in SVGs nicely after all. So back to searching for a library to do this for me. Unfortunately I couldn’t find one. I did find [printpdf](https://docs.rs/printpdf/latest/printpdf/) which is a more manual way of creating a pdf, and has support for SVGs in a feature flag. Unfortunately it uses `usvg` and `svg2pdf` under the hood to do the import.

However it does also give me access to the pdf generation, so I could create a svg without the text in it, and handle the text separately. This is what I ended up doing. Though it wasn’t with out its wrinkles as I’ll describe now.

Step one was to do a depth first search of the svg document tree and find all the text elements. This was easy enough, a recursive tree walk function solves that. Due to rusts memory safety its better to create a new tree to go along with it, so copy everything thats not a text element over to the new tree and add to a vector of text elements. I’m not going to show an example here because its using my svg library and won’t apply anywhere else.

Then using the `printpdf` library we need to create a new document.

```rust
    let (doc, page1, main_layer) = PdfDocument::new(
        "FaerydaeStitches Shape File", 
        Mm(convert::pixels_to_mm(page.width, page.dpi)), 
        Mm(convert::pixels_to_mm(page.height, page.dpi)), 
        "layer1"
    );
    let main_layer_ref = doc.get_page(page1).get_layer(main_layer);
```

Note: we also need to get a `PDFLayerReference` to modify our layer, so we have to look that up after creating it.

Now we can go through and create our text elements. Wait no, to create a text element we need to tell the library what font we are using. First lets go through and create a cache of fonts we need. Now SVGs can’t embed a font, but a pdf can, so we can make sure that our fancy font goes with our documents. This will make them bigger but will also make sure they look right everywhere. I used the [font_kit](https://docs.rs/font-kit/latest/font_kit/) library to handle looking up the path for a system font. I’m only using the `SystemFontSource` bit not any of the rendering.

```rust
fn create_font_cache(doc : &printpdf::PdfDocumentReference, texts: &Vec<TextElement>) -> Result<HashMap<String, IndirectFontRef>, Error> {
    let mut result = HashMap::new();

    for te in texts {
        if !result.contains_key(&te.font_name) {
            let font_path = find_font(te.font_name.clone())?;
            let font = doc.add_external_font(File::open(font_path)?)?;
            result.insert(te.font_name.clone(), font);
        }
    }

    return Ok(result);
}

fn find_font(font_name: String) -> Result<String, Error> {
    let handle = SystemSource::new().select_by_postscript_name(font_name.as_str())?;

    match handle {
        Handle::Path { path, ..} => return Ok(path.to_str().unwrap().to_string()),
        _ => Err(Error::FontMemoryFont)
    }
}
```

Ok Now we have the fonts we can create the text. Awesome…

```Rust
    layer.begin_text_section();

    let font = font_cache.get(&text.font_name).unwrap(); // we created the font cache from this list of text objects. We know this will exist.
    layer.set_font(font, text.font_size as f64);
    
    // set text colours
    layer.set_fill_color(convert_colour(text.fill.as_str())?);
    layer.set_outline_color(convert_colour(text.stroke.as_str())?);

    layer.write_text(text.text.clone(), font);

    layer.end_text_section();
```

Please note: I skipped over the part where we got the colours and font names from the text elements style attributes. Its all just text parsing, `convert_colour` is a helper function to go from `#0CAB0AFF` type hex colour codes to `printpdf`’s rgb colour objects.

Aaaaahhhhh, why is our text at the bottom of the page not the top? PDF’s (0,0) origin point is in the bottom left corner of the page. SVG’s is in the top left. So our text won’t appear where we want. To solve this we need to create a `TextMatrix` to feed our layer. This can just be a translation or rotation or both.

I know I’ll need to do a rotation later so lets solve all of this at once. But, the svg rotation isn’t applied on the text elements. Its applied on a group above the text elements. So when we walk the tree we are going to need to keep track of the current rotation as we go, so we can apply the right rotation values to the text elements we create.

The next fun thing is the coordinates applied to the text element in the svg get transformed by the group above them. So they think they are printing to a normal x,y coordinate plane, but that entire plane then gets rotated by the group. This doesn’t happen in the PDF so we need to undo it. Go from group coordinates to page coordinates. Functionally this is rotating the axis, which is reasonably easy to do with some trigonometry

```rust
// function for finding the coords in the page axis when there has been a rotation applied to the coords
fn get_effective_location(rotation:f64, x:i32, y:i32) -> (i32, i32) {

    if rotation == 0.0 {
        return (x, y)
    }

    let rad_rotation = rotation.to_radians();

    let new_x = ((x as f64) * rad_rotation.cos()) - ((y as f64) * rad_rotation.sin());
    let new_y = ((x as f64) * rad_rotation.sin()) + ((y as f64) * rad_rotation.cos());

    let rx = new_x.round() as i32;
    let ry = new_y.round() as i32;
    return (rx, ry)
}
```

I’m not going to explain how to get here. I don’t want to write another couple of thousand words. It just does what we need and gives us a point in page space as though the group was not there.

Now we need to tell the pdf that this is where we want our text. So we add a `TextMatrix` to our code from before

```rust
    layer.begin_text_section();

    let font = font_cache.get(&text.font_name).unwrap();
    layer.set_font(font, text.font_size as f64);
    // set text rotation
    
    // set text colours
    layer.set_fill_color(convert_colour(text.fill.as_str())?);
    layer.set_outline_color(convert_colour(text.stroke.as_str())?);
    
    layer.set_text_matrix(TextMatrix::TranslateRotate(
        Mm(convert::pixels_to_mm(text.x, page.dpi)).into_pt(),
        convert_y(text.y, page).into_pt(),
        convert_angle(text.rotation)
    ));
    
    
    layer.write_text(text.text.clone(), font);

    layer.end_text_section();
```

The x axis is still the same values, just at the bottom of the page rather than the top. We do need to convert the y axis though. Reasonably straight forward, take the existing y value away from the height of the page, with a bunch of unit conversions thrown in for good measure. My SVGs operate in pixels, the `printpdf` library likes millimeters (its `Mm` objects)

```rust
fn convert_y(y:i32, page: &Page) -> Mm {
    let page_height = convert::pixels_to_mm(page.height, page.dpi);
    let y_mm = convert::pixels_to_mm(y, page.dpi);
    Mm(page_height - y_mm)
}
```

The other wrinkle is because of the flipped axis the rotation angle is different. Instead of positive values rotating clockwise from the top of the page, positive values now rotate counter clockwise from the bottom of the page, again easy to solve, multiply by -1

```rust
fn convert_angle(angle:f64) -> f64 {
    angle * -1.0
}
```

Hurrah! Now our text is the right font, in the right place and going in the right direction. Excellent.

Now we just need to add our svg with out the text elements.

```rust
    let svg_string = svg_filtered.to_pretty_string();
    let pdf_svg = Svg::parse(svg_string.as_str())?;

    let svg_transform = SvgTransform{
        translate_x: None,
        translate_y: None,
        scale_x: None,
        scale_y: None,
        rotate: None,
        dpi: Some(page.dpi as f64),
    };

    pdf_svg.add_to_layer(&main_layer_ref, svg_transform);
```

First we turn our svg object tree into a string so it can be parsed by the pdf library. This is the down side to using our own objects for this, but at least we can do everything we want. Next we parse the string, this is where the text would get converted into paths if we hadn’t removed it all.

We set up a transform, mostly this is just need to set the dpi. If we don’t do this it defaults to 300dpi which is almost certainly wrong. While my code handles any dpi the most common uses are 96 and 72 dpi.

Finally we add the svg to our layer. Tada!

Oh we should probably save our pdf too.

```rust
    Ok(doc.save(&mut BufWriter::new(File::create(path)?))?)
```

Ok now we are finally done. My PDF’s have gone from 1mb ish to 200kb ish. Perfect.

May all this rambling be of no use to you. There is a bunch of stuff I didn’t cover that would be possible to solve. I only handle rotation transforms, not any other kind. It would be easy enough to do so, but my code does not ever generate any thing but rotations. It also doesn’t handle style cascading from group elements and probably a thousand other things. This is one of those problems that the more you look at it the harder it gets.

I’m glad I got this far and am very happy to put it down now. As usual this is as much documentation for my self as any one else.

- 