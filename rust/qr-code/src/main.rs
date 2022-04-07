use image::Rgb;
use qrcode::render::svg;
use qrcode::QrCode;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let website = &args[1];

    let image_name = "qrcode.png";

    // Encode some data into bits.
    let code = QrCode::new(website).unwrap();

    // Render the bits into an image.
    let image = code
        .render::<Rgb<u8>>()
        .dark_color(Rgb([5, 150, 105]))
        .build();

    // Save the image.
    image.save(image_name).unwrap();

    // SVG
    let svg_code = QrCode::new(website).unwrap();
    let svg_image = svg_code
        .render()
        .min_dimensions(200, 200)
        .dark_color(svg::Color("#059669"))
        .light_color(svg::Color("#ffffff"))
        .build();
    println!("{}", svg_image);
}
