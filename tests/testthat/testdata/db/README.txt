Oz butterfly database
---------------------

This folder contains the Oz butterfly database. It consists of this
README.txt file, 2 spreadsheets (each in 3 formats) and sample data
files. Sample data file are stored in folders by family, species and
sample ID.


Summary spreadsheet
-------------------

The summary spreadsheet, Oz_butterflies_summary.csv, .xslx or .json
contains a row for each species in the database and the following
columns:

Species			Binomial species names
Specimens		Number of specimens of the species in the database
Females 		Number of female specimens of the species in the database
Males 			Number of male specimens of the species in the database
Dimorphic		"y" if clear sexual dimorphism in visible or UV, "n" if difficult to ID sexes in visible or UV
Irridescent		"y" if obvious irridescent patches as measured in different angles, "n" if irridescence not clear

Specimen spreadsheet
--------------------

ID              	Specimen identifier
Family                  Specimen family
Genus                   Specimen genus
Species                 Specimen specific name (i.e. not the binomial species name)
Sex                     "Female" or "Male"
Body			"y" or "n" for if body has visible damage
Forewing.left		"y" or "n" for if forewing dorsal has visible damage
Forewing.right		"y" or "n" for if forewing ventral has visible damage
Hindwing.left		"y" or "n" for if hinwing dorsal has visible damage
Hindwing.right		"y" or "n" for if hindwing ventral has visible damage
Site			site code
Latitude		Latitude of site
Longitude		Longitude of site
Climate			"temperate", "subtropical or "tropical"
Date			date of collection in day/month/year format
Collector		Initials of collector
Binomial		Full species name
Pinned                  "y" if specimen is pinned (i.e. intact), "n" if wings are separated from body
Speced			"y" if specimen was speced, "n" if it was not
DNA			"y" if specimen has CO1 sequence in the database, "n" if it does not

Specimen data
-------------

Each specimen folder contains photographs of the specimen in Sony raw fomat (file extension .ARW).
In the following file names, "<ID>" represents the specimen ID.

Pinned (intact) specimens have photographs named:
<ID>-v-RGB.ARW   Ventral wing surfaces photographed in visible light
<ID>-d-RGB.ARW   Dorsal wing surfaces photographed in visible light
<ID>-v-UV.ARW    Ventral wing surfaces photographed in ultraviolet light
<ID>-d-UV.ARW    Dorsal wing surfaces photographed in ultraviolet light

Photographs of non-pinned specimens (wings separated from body):
<ID>-RGB.ARW   Dorsal and ventral wing surfaces photographed in visible light
<ID>-UV.ARW    Dorsal and ventral wing surfaces photographed in ultraviolet light

Specimens with DNA include the following files:
<ID>-f.gb	CO1 gene gene sequenced forward

Spectroscoped specimens include the following files:
<ID>-side-patch number-angle.procspec	where ID is the sample number; side is either "d" (dorsal), "v" (ventra), or "b" (both) that refers to the reference picture; patch number is equivalent to one of the circled patched in the reference photo, and angle is either "s" for standard 45° or "a", 60° or "a2" if the 60° inclination was not enought to show iridescence	
<ID>.csv	csv files containing all procspec measures
<ID>-side.jpeg	reference image for speced patch number. Side can be "d" (dorsal), "v" ventral or NULL for both sides in the same picture 
<ID>-s.png	Ploted specs measured at 45°
<ID>-a.png	Ploted specs measured at 60° or more