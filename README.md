# miRExplorer

This repository contains the data and script related to *"miRExplorer: a Shiny app to investigate miRNAs-disease and miRNA-gene associations"*.

## data

> The Succisa Pratensis molecules are available at `ligand/pdb_file`. For each molecule used in this work we report:

```
- Confomer3D or Structure2D
- Compound_CID (PubChem CID code (Compound ID list))

molecule example : Conformer3D_COMPOUND_CID_88708.pdb
```


> All molecules, converted from .pdb format to .pdbqt, are available at `docking/input`. 

<br/>

## Repositories

>The following repositories contain:

- *data* : contains all files related to molecular docking (input, output, configuration files etc...);
- *ligand* : contains the files for the tested ligands for docking, as they are available online;
- *PyMOL session* : contains PyMOL session save files to analyze docking poses;
- *paper figures* : contains the figures generated and used in the paper;
- *script* : contains the python scripts used to generate some graphs.







