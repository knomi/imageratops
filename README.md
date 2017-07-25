# Imageratops

That beast serves the images off the s3 bucket, scaling and cropping them on demand.

When getting an image, any of the the following optional parameters can be specified:

- `width` (int) -- the desired width of an image
- `height` (int) -- the desired height of an image
- `fit` (`cover` | `contain`) -- the fitting mode

The following rules determine the scaling:
- if only one of the `width`/`height` are specified the other one is calculated to match the original aspect ratio
- if both `width` and `height` are specified, the `fit` parameter specifies how should the image be resized in order to fit the specified box
  - when `fit=cover` (default), the specified box is cropped out from the original image (in order to match the requested aspect ratio) and then scaled (to match the requested size)
  - when `fit=contain`, the image is scaled to be fully contained, fitted into the specified box. Either requested width or height become the resulting width or height, and the other dimensions is calculated to fit the box.

BTW, the `fit` terminology is borrowed from [`object-fit`](https://developer.mozilla.org/en-US/docs/Web/CSS/object-fit).
