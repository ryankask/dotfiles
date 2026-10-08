import AppKit
import CoreText

enum RenderError: Error {
    case bitmapAllocation
    case labelOutsideBadge
    case pngEncoding
    case usage
}

let badgeSize: CGFloat = 16

func makeLine(_ text: String) -> CTLine {
    // Bridge the public system font directly; its private name can fall back
    // to a different font if passed to a PostScript-name lookup.
    let font = NSFont.systemFont(ofSize: 10.5, weight: .semibold) as CTFont
    return CTLineCreateWithAttributedString(NSAttributedString(
        string: text,
        attributes: [
            NSAttributedString.Key(kCTFontAttributeName as String): font,
            NSAttributedString.Key(kCTKernAttributeName as String): 0,
            NSAttributedString.Key(kCTForegroundColorAttributeName as String):
                CGColor(gray: 0, alpha: 1),
        ]
    ))
}

func renderBadge(scale: Int) throws -> CGImage {
    let pixels = Int(badgeSize) * scale
    guard let context = CGContext(
        data: nil, width: pixels, height: pixels, bitsPerComponent: 8,
        bytesPerRow: pixels * 4,
        space: CGColorSpace(name: CGColorSpace.sRGB)!,
        bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue
    ) else {
        throw RenderError.bitmapAllocation
    }
    context.setShouldAntialias(true)
    // Template masks need grayscale coverage, not RGB font smoothing.
    context.setShouldSmoothFonts(false)
    context.setAllowsFontSmoothing(false)
    context.setShouldSubpixelPositionFonts(false)
    context.setShouldSubpixelQuantizeFonts(true)
    let factor = CGFloat(scale)
    context.scaleBy(x: factor, y: factor)
    let canvas = CGRect(x: 0, y: 0, width: badgeSize, height: badgeSize)
    context.setFillColor(CGColor(gray: 0, alpha: 1))
    context.addPath(CGPath(
        roundedRect: canvas, cornerWidth: 3, cornerHeight: 3, transform: nil
    ))
    context.fillPath()

    // The real Ó glyph supplies the kreska. Centre the base letters without
    // counting the accent, and snap separately to each render's pixel grid.
    let line = makeLine("CÓ")
    let bounds = CTLineGetBoundsWithOptions(line, .useGlyphPathBounds)
    let baseBounds = CTLineGetBoundsWithOptions(makeLine("CO"), .useGlyphPathBounds)
    let origin = CGPoint(
        x: ((badgeSize / 2 - bounds.midX) * factor).rounded() / factor,
        y: ((badgeSize / 2 - baseBounds.midY) * factor).rounded() / factor
    )
    guard canvas.insetBy(dx: 0.25, dy: 0.25)
        .contains(bounds.offsetBy(dx: origin.x, dy: origin.y)) else {
        throw RenderError.labelOutsideBadge
    }
    context.textMatrix = .identity
    context.textPosition = origin
    context.setBlendMode(.clear)
    CTLineDraw(line, context)
    guard let image = context.makeImage() else {
        throw RenderError.bitmapAllocation
    }
    return image
}

do {
    guard CommandLine.arguments.count == 2 else {
        throw RenderError.usage
    }
    let iconset = URL(fileURLWithPath: CommandLine.arguments[1], isDirectory: true)
    try FileManager.default.createDirectory(at: iconset, withIntermediateDirectories: true)
    for scale in [1, 2] {
        let bitmap = NSBitmapImageRep(cgImage: try renderBadge(scale: scale))
        bitmap.size = NSSize(width: badgeSize, height: badgeSize)
        guard let data = bitmap.representation(using: .png, properties: [:]) else {
            throw RenderError.pngEncoding
        }
        let filename = scale == 1 ? "icon_16x16.png" : "icon_16x16@2x.png"
        try data.write(to: iconset.appendingPathComponent(filename), options: .atomic)
    }
} catch {
    fputs("Icon rendering failed: \(error)\nUsage: swift render.swift OUTPUT_ICONSET\n", stderr)
    exit(1)
}
