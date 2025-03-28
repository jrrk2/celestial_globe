(* messier_data.ml - Generated from messier_array format *)

type constellation =
| Andromeda
| Aquarius
| Auriga
| Cancer
| Canes_Venatici
| Canis_Major
| Capricornus
| Cassiopeia
| Cetus
| Coma_Berenices
| Cygnus
| Draco
| Gemini
| Hercules
| Hydra
| Leo
| Lepus
| Lyra
| Monoceros
| Ophiuchus
| Orion
| Pegasus
| Perseus
| Pisces
| Puppis
| Sagitta
| Sagittarius
| Scorpius
| Scutum
| Serpens
| Taurus
| Triangulum
| Ursa_Major
| Virgo
| Vulpecula

let constellation_to_string = function
| Andromeda -> "Andromeda"
| Aquarius -> "Aquarius"
| Auriga -> "Auriga"
| Cancer -> "Cancer"
| Canes_Venatici -> "Canes_Venatici"
| Canis_Major -> "Canis_Major"
| Capricornus -> "Capricornus"
| Cassiopeia -> "Cassiopeia"
| Cetus -> "Cetus"
| Coma_Berenices -> "Coma_Berenices"
| Cygnus -> "Cygnus"
| Draco -> "Draco"
| Gemini -> "Gemini"
| Hercules -> "Hercules"
| Hydra -> "Hydra"
| Leo -> "Leo"
| Lepus -> "Lepus"
| Lyra -> "Lyra"
| Monoceros -> "Monoceros"
| Ophiuchus -> "Ophiuchus"
| Orion -> "Orion"
| Pegasus -> "Pegasus"
| Perseus -> "Perseus"
| Pisces -> "Pisces"
| Puppis -> "Puppis"
| Sagitta -> "Sagitta"
| Sagittarius -> "Sagittarius"
| Scorpius -> "Scorpius"
| Scutum -> "Scutum"
| Serpens -> "Serpens"
| Taurus -> "Taurus"
| Triangulum -> "Triangulum"
| Ursa_Major -> "Ursa_Major"
| Virgo -> "Virgo"
| Vulpecula -> "Vulpecula"

(* Messier Object Type *)
type object_type =
  | Globular_Cluster
  | Open_Cluster
  | Nebula
  | Planetary_Nebula
  | Supernova_Remnant
  | Galaxy
  | Galaxy_Cluster
  | Double_Star
  | Asterism
  | Star_Cloud
  | Other

let string_of_object_type = function
  | Globular_Cluster -> "Globular Cluster"
  | Open_Cluster -> "Open Cluster"
  | Nebula -> "Nebula"
  | Planetary_Nebula -> "Planetary Nebula"
  | Supernova_Remnant -> "Supernova Remnant"
  | Galaxy -> "Galaxy"
  | Galaxy_Cluster -> "Galaxy Cluster"
  | Double_Star -> "Double Star"
  | Asterism -> "Asterism"
  | Star_Cloud -> "Star Cloud"
  | Other -> "Other"

(* Messier Object Info *)
type messier_object = {
  id: int;
  name: string;
  common_name: string option;
  object_type: object_type;
  constellation: constellation;
  ra_hours: float;
  dec_degrees: float;
  magnitude: float;
  distance_kly: float;
  size_arcmin: float * float; (* width, height in arcminutes *)
  description: string;
  image_url: string;
  discovery_year: int option;
  best_viewed: string; (* Season or months *)
}

let imaged = [
"M1", false;
"M3", false;
"M13", false;
"M16", false;
"M17", false;
"M27", true;
"M31", false;
"M32", false;
"M34", false;
"M35", false;
"M36", false;
"M37", false;
"M38", false;
"M40", false;
"M41", false;
"M42", true;
"M43", false;
"M44", false;
"M45", true;
"M46", false;
"M47", false;
"M48", false;
"M50", false;
"M51", true;
"M52", false;
"M61", false;
"M63", false;
"M65", false;
"M66", false;
"M67", false;
"M74", true;
"M76", false;
"M77", false;
"M78", false;
"M81", true;
"M82", false;
"M95", false;
"M97", false;
"M100", false;
"M101", true;
"M102", false;
"M103", false;
"M105", false;
"M106", false;
"M108", false;
"M109", true;
"M110", false;
]

(* Complete Messier Catalog Data *)
let catalog = [
  { 
    id = 1;
    name = "M1";
    common_name = Some "Crab Nebula";
    object_type = Supernova_Remnant;
    constellation = Taurus;
    ra_hours = 5.575556;
    dec_degrees = 22.013333;
    magnitude = 20.0;
    distance_kly = 6.5;
    size_arcmin = (6.0, 4.0);
    description = "Remains of a supernova observed in 1054 AD";
    image_url = "images/m1.jpg";
    discovery_year = Some (1731);
    best_viewed = "Winter";
  };
  { 
    id = 2;
    name = "M2";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Aquarius;
    ra_hours = 21.557506;
    dec_degrees = -0.823250;
    magnitude = 6.2;
    distance_kly = 37.5;
    size_arcmin = (16.0, 16.0);
    description = "One of the richest and most compact globular clusters";
    image_url = "images/m2.jpg";
    discovery_year = Some (1746);
    best_viewed = "Autumn";
  };
  { 
    id = 3;
    name = "M3";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Canes_Venatici;
    ra_hours = 13.703228;
    dec_degrees = 28.377278;
    magnitude = 6.4;
    distance_kly = 33.9;
    size_arcmin = (18.0, 18.0);
    description = "Contains approximately 500,000 stars";
    image_url = "images/m3.jpg";
    discovery_year = Some (1764);
    best_viewed = "Spring";
  };
  { 
    id = 4;
    name = "M4";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Scorpius;
    ra_hours = 16.393117;
    dec_degrees = -26.525750;
    magnitude = 20.0;
    distance_kly = 7.2;
    size_arcmin = (26.0, 26.0);
    description = "One of the closest globular clusters to Earth";
    image_url = "images/m4.jpg";
    discovery_year = Some (1746);
    best_viewed = "Summer";
  };
  { 
    id = 5;
    name = "M5";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Serpens;
    ra_hours = 15.309228;
    dec_degrees = 2.081028;
    magnitude = 6.0;
    distance_kly = 24.5;
    size_arcmin = (20.0, 20.0);
    description = "One of the older globular clusters in the Milky Way";
    image_url = "images/m5.jpg";
    discovery_year = Some (1702);
    best_viewed = "Summer";
  };
  { 
    id = 6;
    name = "M6";
    common_name = Some "Butterfly Cluster";
    object_type = Open_Cluster;
    constellation = Scorpius;
    ra_hours = 17.671389;
    dec_degrees = -32.241667;
    magnitude = 20.0;
    distance_kly = 1.6;
    size_arcmin = (25.0, 25.0);
    description = "Contains about 80 stars visible with binoculars";
    image_url = "images/m6.jpg";
    discovery_year = Some (1746);
    best_viewed = "Summer";
  };
  { 
    id = 7;
    name = "M7";
    common_name = Some "Ptolemy's Cluster";
    object_type = Open_Cluster;
    constellation = Scorpius;
    ra_hours = 17.896389;
    dec_degrees = -34.841667;
    magnitude = 20.0;
    distance_kly = 0.8;
    size_arcmin = (80.0, 80.0);
    description = "Mentioned by Ptolemy in 130 AD, visible to naked eye";
    image_url = "images/m7.jpg";
    discovery_year = Some (-130);
    best_viewed = "Summer";
  };
  { 
    id = 8;
    name = "M8";
    common_name = Some "Lagoon Nebula";
    object_type = Nebula;
    constellation = Sagittarius;
    ra_hours = 18.060278;
    dec_degrees = -24.386667;
    magnitude = 20.0;
    distance_kly = 4.1;
    size_arcmin = (90.0, 40.0);
    description = "Contains a distinctive hourglass-shaped structure";
    image_url = "images/m8.jpg";
    discovery_year = Some (1747);
    best_viewed = "Summer";
  };
  { 
    id = 9;
    name = "M9";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Ophiuchus;
    ra_hours = 17.319939;
    dec_degrees = -18.516250;
    magnitude = 8.4;
    distance_kly = 25.8;
    size_arcmin = (9.3, 9.3);
    description = "Located near the center of the Milky Way";
    image_url = "images/m9.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 10;
    name = "M10";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Ophiuchus;
    ra_hours = 16.952514;
    dec_degrees = -4.100306;
    magnitude = 5.0;
    distance_kly = 14.3;
    size_arcmin = (20.0, 20.0);
    description = "One of the brighter globular clusters visible from Earth";
    image_url = "images/m10.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 11;
    name = "M11";
    common_name = Some "Wild Duck Cluster";
    object_type = Open_Cluster;
    constellation = Scutum;
    ra_hours = 18.851111;
    dec_degrees = -6.271667;
    magnitude = 5.8;
    distance_kly = 6.2;
    size_arcmin = (14.0, 14.0);
    description = "Resembles a flight of wild ducks in formation";
    image_url = "images/m11.jpg";
    discovery_year = Some (1681);
    best_viewed = "Summer";
  };
  { 
    id = 12;
    name = "M12";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Ophiuchus;
    ra_hours = 16.787272;
    dec_degrees = -1.948528;
    magnitude = 6.1;
    distance_kly = 16.0;
    size_arcmin = (16.0, 16.0);
    description = "Located in the constellation Ophiuchus";
    image_url = "images/m12.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 13;
    name = "M13";
    common_name = Some "Hercules Globular Cluster";
    object_type = Globular_Cluster;
    constellation = Hercules;
    ra_hours = 16.694898;
    dec_degrees = 36.461319;
    magnitude = 5.8;
    distance_kly = 22.2;
    size_arcmin = (20.0, 20.0);
    description = "Contains several hundred thousand stars";
    image_url = "images/m13.jpg";
    discovery_year = Some (1714);
    best_viewed = "Summer";
  };
  { 
    id = 14;
    name = "M14";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Ophiuchus;
    ra_hours = 17.626708;
    dec_degrees = -3.245917;
    magnitude = 5.7;
    distance_kly = 30.3;
    size_arcmin = (11.0, 11.0);
    description = "One of the more distant globular clusters from Earth";
    image_url = "images/m14.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 15;
    name = "M15";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Pegasus;
    ra_hours = 21.499536;
    dec_degrees = 12.167000;
    magnitude = 20.0;
    distance_kly = 33.6;
    size_arcmin = (18.0, 18.0);
    description = "One of the oldest known globular clusters";
    image_url = "images/m15.jpg";
    discovery_year = Some (1746);
    best_viewed = "Autumn";
  };
  { 
    id = 16;
    name = "M16";
    common_name = Some "Eagle Nebula";
    object_type = Open_Cluster;
    constellation = Serpens;
    ra_hours = 18.312500;
    dec_degrees = -13.791667;
    magnitude = 6.0;
    distance_kly = 7.0;
    size_arcmin = (35.0, 28.0);
    description = "Contains the famous 'Pillars of Creation'";
    image_url = "images/m16.jpg";
    discovery_year = Some (1746);
    best_viewed = "Summer";
  };
  { 
    id = 17;
    name = "M17";
    common_name = Some "Omega Nebula";
    object_type = Nebula;
    constellation = Sagittarius;
    ra_hours = 18.346389;
    dec_degrees = -16.171667;
    magnitude = 20.0;
    distance_kly = 5.0;
    size_arcmin = (11.0, 11.0);
    description = "Also known as the Swan Nebula or Horseshoe Nebula";
    image_url = "images/m17.jpg";
    discovery_year = Some (1745);
    best_viewed = "Summer";
  };
  { 
    id = 18;
    name = "M18";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.332500;
    dec_degrees = -17.088333;
    magnitude = 20.0;
    distance_kly = 4.9;
    size_arcmin = (9.0, 9.0);
    description = "Located in Sagittarius, near other famous deep sky objects";
    image_url = "images/m18.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 19;
    name = "M19";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Ophiuchus;
    ra_hours = 17.043803;
    dec_degrees = -26.267944;
    magnitude = 5.6;
    distance_kly = 28.7;
    size_arcmin = (17.0, 17.0);
    description = "One of the most oblate (flattened) globular clusters";
    image_url = "images/m19.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 20;
    name = "M20";
    common_name = Some "Trifid Nebula";
    object_type = Nebula;
    constellation = Sagittarius;
    ra_hours = 18.045000;
    dec_degrees = -22.971667;
    magnitude = 20.0;
    distance_kly = 5.2;
    size_arcmin = (28.0, 28.0);
    description = "Has a distinctive three-lobed appearance";
    image_url = "images/m20.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 21;
    name = "M21";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.069167;
    dec_degrees = -22.505000;
    magnitude = 20.0;
    distance_kly = 4.2;
    size_arcmin = (13.0, 13.0);
    description = "A relatively young open cluster of stars";
    image_url = "images/m21.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 22;
    name = "M22";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.606650;
    dec_degrees = -23.904750;
    magnitude = 6.2;
    distance_kly = 10.4;
    size_arcmin = (24.0, 24.0);
    description = "One of the brightest globular clusters visible from Earth";
    image_url = "images/m22.jpg";
    discovery_year = Some (1665);
    best_viewed = "Summer";
  };
  { 
    id = 23;
    name = "M23";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Sagittarius;
    ra_hours = 17.949167;
    dec_degrees = -18.986667;
    magnitude = 20.0;
    distance_kly = 2.1;
    size_arcmin = (27.0, 27.0);
    description = "Contains about 150 stars visible with a small telescope";
    image_url = "images/m23.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 24;
    name = "M24";
    common_name = Some "Sagittarius Star Cloud";
    object_type = Star_Cloud;
    constellation = Sagittarius;
    ra_hours = 18.280000;
    dec_degrees = -18.550000;
    magnitude = 20.0;
    distance_kly = 10.0;
    size_arcmin = (90.0, 90.0);
    description = "A dense part of the Milky Way galaxy";
    image_url = "images/m24.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 25;
    name = "M25";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.529167;
    dec_degrees = -19.113333;
    magnitude = 20.0;
    distance_kly = 2.0;
    size_arcmin = (32.0, 32.0);
    description = "Contains about 30 stars visible with binoculars";
    image_url = "images/m25.jpg";
    discovery_year = Some (1745);
    best_viewed = "Summer";
  };
  { 
    id = 26;
    name = "M26";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Scutum;
    ra_hours = 18.754444;
    dec_degrees = -9.386667;
    magnitude = 8.9;
    distance_kly = 5.0;
    size_arcmin = (15.0, 15.0);
    description = "A relatively sparse open cluster in Scutum";
    image_url = "images/m26.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 27;
    name = "M27";
    common_name = Some "Dumbbell Nebula";
    object_type = Planetary_Nebula;
    constellation = Vulpecula;
    ra_hours = 19.993434;
    dec_degrees = 22.721198;
    magnitude = 14.1;
    distance_kly = 1.2;
    size_arcmin = (8.0, 5.7);
    description = "One of the brightest planetary nebulae in the sky";
    image_url = "images/m27.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 28;
    name = "M28";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.409136;
    dec_degrees = -24.869833;
    magnitude = 20.0;
    distance_kly = 18.6;
    size_arcmin = (11.2, 11.2);
    description = "Located in the constellation Sagittarius";
    image_url = "images/m28.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 29;
    name = "M29";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Cygnus;
    ra_hours = 20.396111;
    dec_degrees = 38.486667;
    magnitude = 6.6;
    distance_kly = 4.0;
    size_arcmin = (7.0, 7.0);
    description = "A small but bright cluster in Cygnus";
    image_url = "images/m29.jpg";
    discovery_year = Some (1764);
    best_viewed = "Summer";
  };
  { 
    id = 30;
    name = "M30";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Capricornus;
    ra_hours = 21.672811;
    dec_degrees = -23.179861;
    magnitude = 7.1;
    distance_kly = 26.1;
    size_arcmin = (11.0, 11.0);
    description = "A dense, compact globular cluster";
    image_url = "images/m30.jpg";
    discovery_year = Some (1764);
    best_viewed = "Autumn";
  };
  { 
    id = 31;
    name = "M31";
    common_name = Some "Andromeda Galaxy";
    object_type = Galaxy;
    constellation = Andromeda;
    ra_hours = 0.712314;
    dec_degrees = 41.268750;
    magnitude = 3.4;
    distance_kly = 2500.0;
    size_arcmin = (178.0, 63.0);
    description = "The nearest major galaxy to the Milky Way";
    image_url = "images/m31.jpg";
    discovery_year = Some (964);
    best_viewed = "Autumn";
  };
  { 
    id = 32;
    name = "M32";
    common_name = None;
    object_type = Galaxy;
    constellation = Andromeda;
    ra_hours = 0.711618;
    dec_degrees = 40.865169;
    magnitude = 8.1;
    distance_kly = 2900.0;
    size_arcmin = (8.7, 6.5);
    description = "A satellite galaxy of the Andromeda Galaxy";
    image_url = "images/m32.jpg";
    discovery_year = Some (1749);
    best_viewed = "Autumn";
  };
  { 
    id = 33;
    name = "M33";
    common_name = Some "Triangulum Galaxy";
    object_type = Galaxy;
    constellation = Triangulum;
    ra_hours = 1.564138;
    dec_degrees = 30.660175;
    magnitude = 5.7;
    distance_kly = 2900.0;
    size_arcmin = (73.0, 45.0);
    description = "The third-largest galaxy in the Local Group";
    image_url = "images/m33.jpg";
    discovery_year = Some (1764);
    best_viewed = "Autumn";
  };
  { 
    id = 34;
    name = "M34";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Perseus;
    ra_hours = 2.701944;
    dec_degrees = 42.721667;
    magnitude = 20.0;
    distance_kly = 1.4;
    size_arcmin = (35.0, 35.0);
    description = "Contains about 100 stars and spans 35 light years";
    image_url = "images/m34.jpg";
    discovery_year = Some (1764);
    best_viewed = "Autumn";
  };
  { 
    id = 35;
    name = "M35";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Gemini;
    ra_hours = 6.151389;
    dec_degrees = 24.336667;
    magnitude = 20.0;
    distance_kly = 2.8;
    size_arcmin = (28.0, 28.0);
    description = "A large open cluster visible to the naked eye";
    image_url = "images/m35.jpg";
    discovery_year = Some (1745);
    best_viewed = "Winter";
  };
  { 
    id = 36;
    name = "M36";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Auriga;
    ra_hours = 5.605556;
    dec_degrees = 34.135000;
    magnitude = 6.0;
    distance_kly = 4.1;
    size_arcmin = (12.0, 12.0);
    description = "A young open cluster in Auriga";
    image_url = "images/m36.jpg";
    discovery_year = Some (1764);
    best_viewed = "Winter";
  };
  { 
    id = 37;
    name = "M37";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Auriga;
    ra_hours = 5.871667;
    dec_degrees = 32.545000;
    magnitude = 5.6;
    distance_kly = 4.5;
    size_arcmin = (24.0, 24.0);
    description = "The richest open cluster in Auriga";
    image_url = "images/m37.jpg";
    discovery_year = Some (1764);
    best_viewed = "Winter";
  };
  { 
    id = 38;
    name = "M38";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Auriga;
    ra_hours = 5.477778;
    dec_degrees = 35.823333;
    magnitude = 6.4;
    distance_kly = 4.2;
    size_arcmin = (21.0, 21.0);
    description = "Contains a distinctive cruciform pattern of stars";
    image_url = "images/m38.jpg";
    discovery_year = Some (1764);
    best_viewed = "Winter";
  };
  { 
    id = 39;
    name = "M39";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Cygnus;
    ra_hours = 21.525833;
    dec_degrees = 48.246667;
    magnitude = 20.0;
    distance_kly = 0.8;
    size_arcmin = (32.0, 32.0);
    description = "A loose, scattered open cluster in Cygnus";
    image_url = "images/m39.jpg";
    discovery_year = Some (1764);
    best_viewed = "Autumn";
  };
  { 
    id = 40;
    name = "M40";
    common_name = None;
    object_type = Double_Star;
    constellation = Ursa_Major;
    ra_hours = 12.370000;
    dec_degrees = 58.083333;
    magnitude = 20.0;
    distance_kly = 0.5;
    size_arcmin = (0.8, 0.8);
    description = "Actually a double star system, not a deep sky object";
    image_url = "images/m40.jpg";
    discovery_year = Some (1764);
    best_viewed = "Spring";
  };
  { 
    id = 41;
    name = "M41";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Canis_Major;
    ra_hours = 6.766667;
    dec_degrees = -20.716667;
    magnitude = 4.5;
    distance_kly = 2.3;
    size_arcmin = (38.0, 38.0);
    description = "A bright open cluster easily visible with binoculars";
    image_url = "images/m41.jpg";
    discovery_year = Some (1749);
    best_viewed = "Winter";
  };
  { 
    id = 42;
    name = "M42";
    common_name = Some "Orion Nebula";
    object_type = Nebula;
    constellation = Orion;
    ra_hours = 5.588139;
    dec_degrees = -5.391111;
    magnitude = 20.0;
    distance_kly = 1.3;
    size_arcmin = (85.0, 60.0);
    description = "One of the brightest nebulae visible to the naked eye";
    image_url = "images/m42.jpg";
    discovery_year = Some (1610);
    best_viewed = "Winter";
  };
  { 
    id = 43;
    name = "M43";
    common_name = None;
    object_type = Nebula;
    constellation = Orion;
    ra_hours = 5.591944;
    dec_degrees = -5.270000;
    magnitude = 20.0;
    distance_kly = 1.6;
    size_arcmin = (20.0, 15.0);
    description = "Part of the Orion Nebula complex";
    image_url = "images/m43.jpg";
    discovery_year = Some (1769);
    best_viewed = "Winter";
  };
  { 
    id = 44;
    name = "M44";
    common_name = Some "Beehive Cluster";
    object_type = Open_Cluster;
    constellation = Cancer;
    ra_hours = 8.670278;
    dec_degrees = 19.621667;
    magnitude = 20.0;
    distance_kly = 0.6;
    size_arcmin = (95.0, 95.0);
    description = "Also known as Praesepe, visible to naked eye";
    image_url = "images/m44.jpg";
    discovery_year = Some (-260);
    best_viewed = "Winter";
  };
  { 
    id = 45;
    name = "M45";
    common_name = Some "Pleiades";
    object_type = Open_Cluster;
    constellation = Taurus;
    ra_hours = 3.773333;
    dec_degrees = 24.113333;
    magnitude = 20.0;
    distance_kly = 0.4;
    size_arcmin = (110.0, 110.0);
    description = "The Seven Sisters, visible to naked eye";
    image_url = "images/m45.jpg";
    discovery_year = Some (-1000);
    best_viewed = "Winter";
  };
  { 
    id = 46;
    name = "M46";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Puppis;
    ra_hours = 7.696389;
    dec_degrees = -14.843333;
    magnitude = 20.0;
    distance_kly = 5.4;
    size_arcmin = (27.0, 27.0);
    description = "Contains a planetary nebula within the cluster";
    image_url = "images/m46.jpg";
    discovery_year = Some (1771);
    best_viewed = "Winter";
  };
  { 
    id = 47;
    name = "M47";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Puppis;
    ra_hours = 7.609722;
    dec_degrees = -14.488333;
    magnitude = 20.0;
    distance_kly = 1.6;
    size_arcmin = (30.0, 30.0);
    description = "A bright, large open cluster in Puppis";
    image_url = "images/m47.jpg";
    discovery_year = Some (1771);
    best_viewed = "Winter";
  };
  { 
    id = 48;
    name = "M48";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Hydra;
    ra_hours = 8.227500;
    dec_degrees = -5.726667;
    magnitude = 20.0;
    distance_kly = 1.5;
    size_arcmin = (54.0, 54.0);
    description = "A large open cluster visible with binoculars";
    image_url = "images/m48.jpg";
    discovery_year = Some (1771);
    best_viewed = "Winter";
  };
  { 
    id = 49;
    name = "M49";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.496333;
    dec_degrees = 8.000411;
    magnitude = 12.2;
    distance_kly = 56000.0;
    size_arcmin = (9.0, 7.5);
    description = "An elliptical galaxy in the Virgo Cluster";
    image_url = "images/m49.jpg";
    discovery_year = Some (1771);
    best_viewed = "Spring";
  };
  { 
    id = 50;
    name = "M50";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Monoceros;
    ra_hours = 7.046528;
    dec_degrees = -8.337778;
    magnitude = 20.0;
    distance_kly = 3.0;
    size_arcmin = (16.0, 16.0);
    description = "Contains about 200 stars in a heart-shaped pattern";
    image_url = "images/m50.jpg";
    discovery_year = Some (1772);
    best_viewed = "Winter";
  };
  { 
    id = 51;
    name = "M51";
    common_name = Some "Whirlpool Galaxy";
    object_type = Galaxy;
    constellation = Canes_Venatici;
    ra_hours = 13.497972;
    dec_degrees = 47.195258;
    magnitude = 8.4;
    distance_kly = 23000.0;
    size_arcmin = (11.2, 6.9);
    description = "A classic example of a spiral galaxy";
    image_url = "images/m51.jpg";
    discovery_year = Some (1773);
    best_viewed = "Spring";
  };
  { 
    id = 52;
    name = "M52";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Cassiopeia;
    ra_hours = 23.413056;
    dec_degrees = 61.590000;
    magnitude = 20.0;
    distance_kly = 5.0;
    size_arcmin = (13.0, 13.0);
    description = "A rich open cluster in Cassiopeia";
    image_url = "images/m52.jpg";
    discovery_year = Some (1774);
    best_viewed = "Autumn";
  };
  { 
    id = 53;
    name = "M53";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Coma_Berenices;
    ra_hours = 13.215347;
    dec_degrees = 18.168167;
    magnitude = 7.8;
    distance_kly = 58.0;
    size_arcmin = (13.0, 13.0);
    description = "A globular cluster in the constellation Coma Berenices";
    image_url = "images/m53.jpg";
    discovery_year = Some (1775);
    best_viewed = "Spring";
  };
  { 
    id = 54;
    name = "M54";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.917592;
    dec_degrees = -30.479861;
    magnitude = 20.0;
    distance_kly = 87.4;
    size_arcmin = (9.1, 9.1);
    description = "A small, dense globular cluster in Sagittarius";
    image_url = "images/m54.jpg";
    discovery_year = Some (1778);
    best_viewed = "Summer";
  };
  { 
    id = 55;
    name = "M55";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagittarius;
    ra_hours = 19.666586;
    dec_degrees = -30.964750;
    magnitude = 6.5;
    distance_kly = 17.3;
    size_arcmin = (19.0, 19.0);
    description = "A large, bright globular cluster";
    image_url = "images/m55.jpg";
    discovery_year = Some (1778);
    best_viewed = "Summer";
  };
  { 
    id = 56;
    name = "M56";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Lyra;
    ra_hours = 19.276547;
    dec_degrees = 30.183472;
    magnitude = 20.0;
    distance_kly = 32.9;
    size_arcmin = (7.1, 7.1);
    description = "A moderately concentrated globular cluster";
    image_url = "images/m56.jpg";
    discovery_year = Some (1779);
    best_viewed = "Summer";
  };
  { 
    id = 57;
    name = "M57";
    common_name = Some "Ring Nebula";
    object_type = Planetary_Nebula;
    constellation = Lyra;
    ra_hours = 18.893082;
    dec_degrees = 33.029134;
    magnitude = 15.8;
    distance_kly = 2.3;
    size_arcmin = (1.4, 1.0);
    description = "A classic planetary nebula with a ring-like appearance";
    image_url = "images/m57.jpg";
    discovery_year = Some (1779);
    best_viewed = "Summer";
  };
  { 
    id = 58;
    name = "M58";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.628777;
    dec_degrees = 11.818089;
    magnitude = 9.7;
    distance_kly = 62.0;
    size_arcmin = (5.9, 4.7);
    description = "A barred spiral galaxy in the Virgo Cluster";
    image_url = "images/m58.jpg";
    discovery_year = Some (1779);
    best_viewed = "Spring";
  };
  { 
    id = 59;
    name = "M59";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.700627;
    dec_degrees = 11.646919;
    magnitude = 20.0;
    distance_kly = 60.0;
    size_arcmin = (5.4, 3.7);
    description = "An elliptical galaxy in the Virgo Cluster";
    image_url = "images/m59.jpg";
    discovery_year = Some (1779);
    best_viewed = "Spring";
  };
  { 
    id = 60;
    name = "M60";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.727770;
    dec_degrees = 11.552691;
    magnitude = 20.0;
    distance_kly = 55.0;
    size_arcmin = (7.6, 6.2);
    description = "A large elliptical galaxy interacting with NGC 4647";
    image_url = "images/m60.jpg";
    discovery_year = Some (1779);
    best_viewed = "Spring";
  };
  { 
    id = 61;
    name = "M61";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.365258;
    dec_degrees = 4.473777;
    magnitude = 9.7;
    distance_kly = 52.5;
    size_arcmin = (6.5, 5.9);
    description = "A spiral galaxy in the Virgo Cluster";
    image_url = "images/m61.jpg";
    discovery_year = Some (1779);
    best_viewed = "Spring";
  };
  { 
    id = 62;
    name = "M62";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Ophiuchus;
    ra_hours = 17.020167;
    dec_degrees = -30.112361;
    magnitude = 7.4;
    distance_kly = 22.5;
    size_arcmin = (15.0, 15.0);
    description = "A compact globular cluster near the galactic center";
    image_url = "images/m62.jpg";
    discovery_year = Some (1771);
    best_viewed = "Summer";
  };
  { 
    id = 63;
    name = "M63";
    common_name = Some "Sunflower Galaxy";
    object_type = Galaxy;
    constellation = Canes_Venatici;
    ra_hours = 13.263687;
    dec_degrees = 42.029369;
    magnitude = 8.6;
    distance_kly = 37.0;
    size_arcmin = (12.6, 7.2);
    description = "A spiral galaxy with well-defined arms";
    image_url = "images/m63.jpg";
    discovery_year = Some (1779);
    best_viewed = "Spring";
  };
  { 
    id = 64;
    name = "M64";
    common_name = Some "Black Eye Galaxy";
    object_type = Galaxy;
    constellation = Coma_Berenices;
    ra_hours = 12.945471;
    dec_degrees = 21.682658;
    magnitude = 8.5;
    distance_kly = 24.0;
    size_arcmin = (9.3, 5.4);
    description = "Has a dark band of dust in front of its nucleus";
    image_url = "images/m64.jpg";
    discovery_year = Some (1779);
    best_viewed = "Spring";
  };
  { 
    id = 65;
    name = "M65";
    common_name = None;
    object_type = Galaxy;
    constellation = Leo;
    ra_hours = 11.315530;
    dec_degrees = 13.092306;
    magnitude = 20.0;
    distance_kly = 35.0;
    size_arcmin = (9.8, 2.9);
    description = "Member of the Leo Triplet group of galaxies";
    image_url = "images/m65.jpg";
    discovery_year = Some (1780);
    best_viewed = "Spring";
  };
  { 
    id = 66;
    name = "M66";
    common_name = None;
    object_type = Galaxy;
    constellation = Leo;
    ra_hours = 11.337507;
    dec_degrees = 12.991289;
    magnitude = 8.9;
    distance_kly = 35.0;
    size_arcmin = (9.1, 4.2);
    description = "Member of the Leo Triplet group of galaxies";
    image_url = "images/m66.jpg";
    discovery_year = Some (1780);
    best_viewed = "Spring";
  };
  { 
    id = 67;
    name = "M67";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Cancer;
    ra_hours = 8.856389;
    dec_degrees = 11.813333;
    magnitude = 20.0;
    distance_kly = 2.7;
    size_arcmin = (30.0, 30.0);
    description = "One of the oldest known open clusters";
    image_url = "images/m67.jpg";
    discovery_year = Some (1779);
    best_viewed = "Winter";
  };
  { 
    id = 68;
    name = "M68";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Hydra;
    ra_hours = 12.657772;
    dec_degrees = -26.744056;
    magnitude = 8.0;
    distance_kly = 33.6;
    size_arcmin = (12.0, 12.0);
    description = "A globular cluster in the constellation Hydra";
    image_url = "images/m68.jpg";
    discovery_year = Some (1780);
    best_viewed = "Spring";
  };
  { 
    id = 69;
    name = "M69";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.523083;
    dec_degrees = -32.348083;
    magnitude = 8.3;
    distance_kly = 29.7;
    size_arcmin = (7.1, 7.1);
    description = "A globular cluster near the galactic center";
    image_url = "images/m69.jpg";
    discovery_year = Some (1780);
    best_viewed = "Summer";
  };
  { 
    id = 70;
    name = "M70";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagittarius;
    ra_hours = 18.720211;
    dec_degrees = -32.292111;
    magnitude = 9.1;
    distance_kly = 29.4;
    size_arcmin = (7.8, 7.8);
    description = "A compact globular cluster in Sagittarius";
    image_url = "images/m70.jpg";
    discovery_year = Some (1780);
    best_viewed = "Summer";
  };
  { 
    id = 71;
    name = "M71";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagitta;
    ra_hours = 19.896247;
    dec_degrees = 18.779194;
    magnitude = 6.1;
    distance_kly = 13.0;
    size_arcmin = (7.2, 7.2);
    description = "A loose globular cluster, once considered an open cluster";
    image_url = "images/m71.jpg";
    discovery_year = Some (1780);
    best_viewed = "Summer";
  };
  { 
    id = 72;
    name = "M72";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Aquarius;
    ra_hours = 20.891028;
    dec_degrees = -12.537306;
    magnitude = 9.0;
    distance_kly = 53.4;
    size_arcmin = (6.6, 6.6);
    description = "A fairly dim and distant globular cluster";
    image_url = "images/m72.jpg";
    discovery_year = Some (1780);
    best_viewed = "Summer";
  };
  { 
    id = 73;
    name = "M73";
    common_name = None;
    object_type = Asterism;
    constellation = Aquarius;
    ra_hours = 20.983333;
    dec_degrees = -12.633333;
    magnitude = 8.9;
    distance_kly = 2.0;
    size_arcmin = (2.5, 2.5);
    description = "A group of four stars, not a true deep sky object";
    image_url = "images/m73.jpg";
    discovery_year = Some (1780);
    best_viewed = "Summer";
  };
  { 
    id = 74;
    name = "M74";
    common_name = None;
    object_type = Galaxy;
    constellation = Pisces;
    ra_hours = 1.611596;
    dec_degrees = 15.783641;
    magnitude = 9.5;
    distance_kly = 32.0;
    size_arcmin = (10.2, 9.5);
    description = "A face-on spiral galaxy with well-defined arms";
    image_url = "images/m74.jpg";
    discovery_year = Some (1780);
    best_viewed = "Autumn";
  };
  { 
    id = 75;
    name = "M75";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Sagittarius;
    ra_hours = 20.101345;
    dec_degrees = -21.922261;
    magnitude = 8.3;
    distance_kly = 67.5;
    size_arcmin = (6.8, 6.8);
    description = "A compact, dense globular cluster";
    image_url = "images/m75.jpg";
    discovery_year = Some (1780);
    best_viewed = "Summer";
  };
  { 
    id = 76;
    name = "M76";
    common_name = Some "Little Dumbbell Nebula";
    object_type = Planetary_Nebula;
    constellation = Perseus;
    ra_hours = 1.705460;
    dec_degrees = 51.575426;
    magnitude = 17.5;
    distance_kly = 3.4;
    size_arcmin = (2.7, 1.8);
    description = "A small, faint planetary nebula";
    image_url = "images/m76.jpg";
    discovery_year = Some (1780);
    best_viewed = "Autumn";
  };
  { 
    id = 77;
    name = "M77";
    common_name = None;
    object_type = Galaxy;
    constellation = Cetus;
    ra_hours = 2.711308;
    dec_degrees = -0.013294;
    magnitude = 8.9;
    distance_kly = 47.0;
    size_arcmin = (7.1, 6.0);
    description = "A barred spiral galaxy and Seyfert galaxy";
    image_url = "images/m77.jpg";
    discovery_year = Some (1780);
    best_viewed = "Autumn";
  };
  { 
    id = 78;
    name = "M78";
    common_name = None;
    object_type = Nebula;
    constellation = Orion;
    ra_hours = 5.779389;
    dec_degrees = 0.079167;
    magnitude = 20.0;
    distance_kly = 1.6;
    size_arcmin = (8.0, 6.0);
    description = "A reflection nebula in the constellation Orion";
    image_url = "images/m78.jpg";
    discovery_year = Some (1780);
    best_viewed = "Winter";
  };
  { 
    id = 79;
    name = "M79";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Lepus;
    ra_hours = 5.402942;
    dec_degrees = -24.524250;
    magnitude = 8.2;
    distance_kly = 42.1;
    size_arcmin = (8.7, 8.7);
    description = "An unusual globular cluster that may have originated outside our galaxy";
    image_url = "images/m79.jpg";
    discovery_year = Some (1780);
    best_viewed = "Winter";
  };
  { 
    id = 80;
    name = "M80";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Scorpius;
    ra_hours = 16.284003;
    dec_degrees = -22.976083;
    magnitude = 20.0;
    distance_kly = 32.6;
    size_arcmin = (10.0, 10.0);
    description = "A dense, compact globular cluster";
    image_url = "images/m80.jpg";
    discovery_year = Some (1781);
    best_viewed = "Summer";
  };
  { 
    id = 81;
    name = "M81";
    common_name = Some "Bode's Galaxy";
    object_type = Galaxy;
    constellation = Ursa_Major;
    ra_hours = 9.925881;
    dec_degrees = 69.065295;
    magnitude = 6.9;
    distance_kly = 11.8;
    size_arcmin = (26.9, 14.1);
    description = "A grand design spiral galaxy";
    image_url = "images/m81.jpg";
    discovery_year = Some (1774);
    best_viewed = "Spring";
  };
  { 
    id = 82;
    name = "M82";
    common_name = Some "Cigar Galaxy";
    object_type = Galaxy;
    constellation = Ursa_Major;
    ra_hours = 9.931231;
    dec_degrees = 69.679703;
    magnitude = 8.4;
    distance_kly = 12.0;
    size_arcmin = (11.2, 4.3);
    description = "A starburst galaxy with intense star formation";
    image_url = "images/m82.jpg";
    discovery_year = Some (1774);
    best_viewed = "Spring";
  };
  { 
    id = 83;
    name = "M83";
    common_name = Some "Southern Pinwheel Galaxy";
    object_type = Galaxy;
    constellation = Hydra;
    ra_hours = 13.616922;
    dec_degrees = -29.865761;
    magnitude = 7.5;
    distance_kly = 15.0;
    size_arcmin = (12.9, 11.5);
    description = "A face-on spiral galaxy visible from southern hemisphere";
    image_url = "images/m83.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 84;
    name = "M84";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.417706;
    dec_degrees = 12.886983;
    magnitude = 10.5;
    distance_kly = 60.0;
    size_arcmin = (6.5, 5.6);
    description = "A lenticular galaxy in the Virgo Cluster";
    image_url = "images/m84.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 85;
    name = "M85";
    common_name = None;
    object_type = Galaxy;
    constellation = Coma_Berenices;
    ra_hours = 12.423348;
    dec_degrees = 18.191081;
    magnitude = 20.0;
    distance_kly = 60.0;
    size_arcmin = (7.1, 5.2);
    description = "A lenticular galaxy in the Virgo Cluster";
    image_url = "images/m85.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 86;
    name = "M86";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.436615;
    dec_degrees = 12.945969;
    magnitude = 8.9;
    distance_kly = 52.0;
    size_arcmin = (8.9, 5.8);
    description = "A lenticular galaxy in the Virgo Cluster";
    image_url = "images/m86.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 87;
    name = "M87";
    common_name = Some "Virgo A";
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.513729;
    dec_degrees = 12.391123;
    magnitude = 8.6;
    distance_kly = 53.5;
    size_arcmin = (8.3, 6.6);
    description = "A supergiant elliptical galaxy with active nucleus";
    image_url = "images/m87.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 88;
    name = "M88";
    common_name = None;
    object_type = Galaxy;
    constellation = Coma_Berenices;
    ra_hours = 12.533098;
    dec_degrees = 14.420319;
    magnitude = 13.2;
    distance_kly = 60.0;
    size_arcmin = (6.9, 3.7);
    description = "A spiral galaxy in the Virgo Cluster";
    image_url = "images/m88.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 89;
    name = "M89";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.594391;
    dec_degrees = 12.556342;
    magnitude = 9.8;
    distance_kly = 60.0;
    size_arcmin = (5.1, 4.2);
    description = "An elliptical galaxy in the Virgo Cluster";
    image_url = "images/m89.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 90;
    name = "M90";
    common_name = None;
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.613834;
    dec_degrees = 13.162923;
    magnitude = 9.5;
    distance_kly = 60.0;
    size_arcmin = (9.5, 4.4);
    description = "A spiral galaxy in the Virgo Cluster";
    image_url = "images/m90.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 91;
    name = "M91";
    common_name = None;
    object_type = Galaxy;
    constellation = Coma_Berenices;
    ra_hours = 12.590679;
    dec_degrees = 14.496322;
    magnitude = 13.6;
    distance_kly = 63.0;
    size_arcmin = (5.4, 4.4);
    description = "A barred spiral galaxy in the Virgo Cluster";
    image_url = "images/m91.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 92;
    name = "M92";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Hercules;
    ra_hours = 17.285386;
    dec_degrees = 43.135944;
    magnitude = 6.5;
    distance_kly = 26.7;
    size_arcmin = (14.0, 14.0);
    description = "A bright globular cluster in Hercules";
    image_url = "images/m92.jpg";
    discovery_year = Some (1777);
    best_viewed = "Summer";
  };
  { 
    id = 93;
    name = "M93";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Puppis;
    ra_hours = 7.742778;
    dec_degrees = -23.853333;
    magnitude = 20.0;
    distance_kly = 3.6;
    size_arcmin = (22.0, 22.0);
    description = "A bright open cluster with about 80 stars";
    image_url = "images/m93.jpg";
    discovery_year = Some (1781);
    best_viewed = "Winter";
  };
  { 
    id = 94;
    name = "M94";
    common_name = None;
    object_type = Galaxy;
    constellation = Canes_Venatici;
    ra_hours = 12.848076;
    dec_degrees = 41.120250;
    magnitude = 8.2;
    distance_kly = 16.0;
    size_arcmin = (11.2, 9.1);
    description = "A spiral galaxy with a bright central region";
    image_url = "images/m94.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 95;
    name = "M95";
    common_name = None;
    object_type = Galaxy;
    constellation = Leo;
    ra_hours = 10.732703;
    dec_degrees = 11.703695;
    magnitude = 9.7;
    distance_kly = 38.0;
    size_arcmin = (7.4, 5.0);
    description = "A barred spiral galaxy in the Leo I group";
    image_url = "images/m95.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 96;
    name = "M96";
    common_name = None;
    object_type = Galaxy;
    constellation = Leo;
    ra_hours = 10.779373;
    dec_degrees = 11.819939;
    magnitude = 9.2;
    distance_kly = 31.0;
    size_arcmin = (7.6, 5.2);
    description = "A spiral galaxy in the Leo I group";
    image_url = "images/m96.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 97;
    name = "M97";
    common_name = Some "Owl Nebula";
    object_type = Planetary_Nebula;
    constellation = Ursa_Major;
    ra_hours = 11.246587;
    dec_degrees = 55.019023;
    magnitude = 15.8;
    distance_kly = 2.0;
    size_arcmin = (3.4, 3.3);
    description = "A planetary nebula that resembles an owl's face";
    image_url = "images/m97.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 98;
    name = "M98";
    common_name = None;
    object_type = Galaxy;
    constellation = Coma_Berenices;
    ra_hours = 12.230081;
    dec_degrees = 14.900543;
    magnitude = 10.1;
    distance_kly = 60.0;
    size_arcmin = (9.8, 2.8);
    description = "A spiral galaxy in the Virgo Cluster";
    image_url = "images/m98.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 99;
    name = "M99";
    common_name = None;
    object_type = Galaxy;
    constellation = Coma_Berenices;
    ra_hours = 12.313785;
    dec_degrees = 14.416489;
    magnitude = 9.9;
    distance_kly = 60.0;
    size_arcmin = (5.4, 4.8);
    description = "A nearly face-on spiral galaxy in the Virgo Cluster";
    image_url = "images/m99.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 100;
    name = "M100";
    common_name = None;
    object_type = Galaxy;
    constellation = Coma_Berenices;
    ra_hours = 12.381925;
    dec_degrees = 15.822305;
    magnitude = 9.3;
    distance_kly = 55.0;
    size_arcmin = (7.4, 6.3);
    description = "A grand design spiral galaxy in the Virgo Cluster";
    image_url = "images/m100.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 101;
    name = "M101";
    common_name = Some "Pinwheel Galaxy";
    object_type = Galaxy;
    constellation = Ursa_Major;
    ra_hours = 14.053495;
    dec_degrees = 54.348750;
    magnitude = 7.9;
    distance_kly = 27.0;
    size_arcmin = (28.8, 26.9);
    description = "A face-on spiral galaxy with prominent arms";
    image_url = "images/m101.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 102;
    name = "M102";
    common_name = None;
    object_type = Galaxy;
    constellation = Draco;
    ra_hours = 15.108211;
    dec_degrees = 55.763308;
    magnitude = 9.9;
    distance_kly = 30.0;
    size_arcmin = (5.2, 2.3);
    description = "A lenticular or spiral galaxy in Draco";
    image_url = "images/m102.jpg";
    discovery_year = Some (1781);
    best_viewed = "Summer";
  };
  { 
    id = 103;
    name = "M103";
    common_name = None;
    object_type = Open_Cluster;
    constellation = Cassiopeia;
    ra_hours = 1.555833;
    dec_degrees = 60.658333;
    magnitude = 7.4;
    distance_kly = 8.5;
    size_arcmin = (6.0, 6.0);
    description = "A relatively young open cluster in Cassiopeia";
    image_url = "images/m103.jpg";
    discovery_year = Some (1781);
    best_viewed = "Autumn";
  };
  { 
    id = 104;
    name = "M104";
    common_name = Some "Sombrero Galaxy";
    object_type = Galaxy;
    constellation = Virgo;
    ra_hours = 12.666508;
    dec_degrees = -11.623052;
    magnitude = 8.0;
    distance_kly = 29.3;
    size_arcmin = (8.7, 3.5);
    description = "A galaxy with a distinctive dust lane like a sombrero";
    image_url = "images/m104.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 105;
    name = "M105";
    common_name = None;
    object_type = Galaxy;
    constellation = Leo;
    ra_hours = 10.797111;
    dec_degrees = 12.581631;
    magnitude = 9.8;
    distance_kly = 32.0;
    size_arcmin = (5.4, 4.8);
    description = "An elliptical galaxy in the Leo I group";
    image_url = "images/m105.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 106;
    name = "M106";
    common_name = None;
    object_type = Galaxy;
    constellation = Canes_Venatici;
    ra_hours = 12.316006;
    dec_degrees = 47.303719;
    magnitude = 8.4;
    distance_kly = 22.8;
    size_arcmin = (18.6, 7.6);
    description = "A spiral galaxy with an active galactic nucleus";
    image_url = "images/m106.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 107;
    name = "M107";
    common_name = None;
    object_type = Globular_Cluster;
    constellation = Ophiuchus;
    ra_hours = 16.542183;
    dec_degrees = -13.053778;
    magnitude = 8.8;
    distance_kly = 20.9;
    size_arcmin = (13.0, 13.0);
    description = "A globular cluster in Ophiuchus";
    image_url = "images/m107.jpg";
    discovery_year = Some (1782);
    best_viewed = "Summer";
  };
  { 
    id = 108;
    name = "M108";
    common_name = None;
    object_type = Galaxy;
    constellation = Ursa_Major;
    ra_hours = 11.191935;
    dec_degrees = 55.674122;
    magnitude = 20.0;
    distance_kly = 45.0;
    size_arcmin = (8.7, 2.2);
    description = "An edge-on barred spiral galaxy near the Big Dipper";
    image_url = "images/m108.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 109;
    name = "M109";
    common_name = None;
    object_type = Galaxy;
    constellation = Ursa_Major;
    ra_hours = 11.959990;
    dec_degrees = 53.374724;
    magnitude = 20.0;
    distance_kly = 55.0;
    size_arcmin = (7.6, 4.7);
    description = "A barred spiral galaxy in Ursa Major";
    image_url = "images/m109.jpg";
    discovery_year = Some (1781);
    best_viewed = "Spring";
  };
  { 
    id = 110;
    name = "M110";
    common_name = None;
    object_type = Galaxy;
    constellation = Andromeda;
    ra_hours = 0.672794;
    dec_degrees = 41.685419;
    magnitude = 8.1;
    distance_kly = 2.2;
    size_arcmin = (21.9, 11.0);
    description = "A satellite galaxy of the Andromeda Galaxy";
    image_url = "images/m110.jpg";
    discovery_year = Some (1773);
    best_viewed = "Autumn";
  };
]

(* Helper functions *)
let find_by_id id =
  List.find_opt (fun obj -> obj.id = id) catalog

let filter_by_object_type objtype =
  List.filter (fun obj -> obj.object_type = objtype) catalog

let filter_by_constellation const =
  List.filter (fun obj -> obj.constellation = const) catalog

let filter_by_magnitude mag_limit =
  List.filter (fun obj -> obj.magnitude <= mag_limit) catalog

let all_constellations () =
  let consts = List.map (fun obj -> obj.constellation) catalog in
  List.sort_uniq compare consts

let coordinates_to_string obj =
  let ra_h = int_of_float obj.ra_hours in
  let ra_m = int_of_float ((obj.ra_hours -. float_of_int ra_h) *. 60.0) in
  let ra_s = ((obj.ra_hours -. float_of_int ra_h) *. 60.0 -. float_of_int ra_m) *. 60.0 in
  
  let dec_d = int_of_float obj.dec_degrees in
  let dec_m = int_of_float (abs_float (obj.dec_degrees -. float_of_int dec_d) *. 60.0) in
  let dec_s = (abs_float (obj.dec_degrees -. float_of_int dec_d) *. 60.0 -. float_of_int dec_m) *. 60.0 in
  
  let dec_sign = if obj.dec_degrees < 0.0 then "-" else "+" in
  
  Printf.sprintf "RA: %02dh %02dm %04.1fs, Dec: %s%02d° %02d' %04.1f\"" 
    ra_h ra_m ra_s dec_sign (abs dec_d) dec_m dec_s
