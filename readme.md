# Enhanced Celestial Globe with Texture Atlases

This project enhances the WebGL celestial globe by implementing texture atlases to increase the number of visible Messier objects. By combining related textures into atlases, we can overcome WebGL texture unit limitations and display more objects simultaneously.

## Main Features

1. **Texture Atlas Generation** - Organizes nearby Messier objects into clusters and generates texture atlases.
2. **Celestial Clustering** - Groups objects based on their celestial coordinates to minimize the number of required textures.
3. **WebGL Rendering Enhancement** - Modified WebGL shaders to support texture atlases with multiple objects per texture.
4. **Optimized Resource Usage** - More efficient use of WebGL resources to display a larger number of objects.

## How It Works

1. **Celestial Clustering Algorithm**:
   - Groups Messier objects based on their proximity in the sky
   - Calculates optimal center points for each cluster
   - Limits each cluster to a maximum of 4 objects (for 2x2 grid textures)

2. **Texture Atlas Generation**:
   - Creates 2x2 grid layouts for each cluster's objects
   - Maps each object to a specific region within the atlas
   - Generates instructions for creating the actual atlas images

3. **Enhanced Rendering**:
   - Modified shaders to sample from the correct part of each atlas
   - Improved object positioning and display

## Setup Instructions

### Building the Project

1. Install the required dependencies:
   ```
   opam install js_of_ocaml js_of_ocaml-lwt
   ```

2. Build the project using dune:
   ```
   dune build
   ```

### Generating Texture Atlases

There are two ways to generate the texture atlases:

#### Option 1: Using the build script (recommended)

1. Use the provided build script which handles everything automatically:
   ```
   ./build_atlases.sh --source-dir=/path/to/messier/images --output-dir=/path/to/output
   ```

   The script will check for requirements, generate a Makefile, and then build all the texture atlases in parallel.

2. Copy the generated code for `messier_objects` from the Makefile into `webgldemo.ml`.

#### Option 2: Manual Makefile generation

1. Generate the Makefile for atlas creation:
   ```
   dune exec -- atlas_generator_makefile --source-dir=/path/to/messier/images --output-dir=/path/to/output
   ```

2. Build the texture atlases using the generated Makefile:
   ```
   make -f Makefile.atlas
   ```

3. Copy the generated code for `messier_objects` from the Makefile into `webgldemo.ml`.

### Running the Application

- Start a local web server in the project directory, for example:
  ```
  python -m http.server 9000
  ```

- Open your browser and navigate to http://localhost:9000

## Usage Modes

The application supports three different modes:

1. **Standard Globe Mode**:
   ```
   dune exec -- webgldemo --standard
   ```

2. **Celestial Globe Mode** (original, with individual textures):
   ```
   dune exec -- webgldemo --celestial
   ```

3. **Atlas Globe Mode** (enhanced, with texture atlases):
   ```
   dune exec -- webgldemo --atlas
   ```

## Configuration

### Image Processing Parameters

The atlas generation process supports several important parameters:

1. **Clustering Parameters**:
   ```
   --max-distance=40.0   # Maximum angular distance between objects in a cluster
   --max-per-cluster=4   # Maximum number of objects per cluster
   ```

2. **Processing Parameters**:
   ```
   --arcsec-per-pixel=2.0  # Used only for sizing large objects
   ```

The `arcsec-per-pixel` parameter is now used primarily to identify and properly handle large objects:

- It helps determine which objects are too large for standard 512x512 display
- For large objects (like M31), it calculates appropriate scaling while preserving aspect ratio
- For standard objects, the processing focuses on trimming excess background and centering

Example of adjusting parameters:
```
./build_atlases.sh --max-distance=30 --max-objects=4 --arcsec-per-pixel=1.5
```

## Troubleshooting

1. **Texture Loading Issues**: Check the texture loading log in the UI. You may need to adjust the path to texture files in the code.

2. **Low Performance**: Try reducing the number of clusters or the sphere resolution (in `generate_sphere` function).

3. **WebGL Errors**: The application logs WebGL errors to the console and the texture log. Make sure your browser supports WebGL 1.0.

## Extensions

This implementation can be extended in several ways:

1. Support for more complex atlas layouts (3x3, 4x4)
2. Dynamic loading of textures based on view angle
3. Hierarchical clustering for better organization of the night sky
4. Support for additional object catalogs beyond Messier

## Credits

This project enhances the original WebGL celestial globe implementation with texture atlas support to overcome the limitations of WebGL texture units.
