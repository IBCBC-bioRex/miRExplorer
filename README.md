# Succisa Pratensis - molecular docking 

This repository contains the data and script used for the molecular docking of *Succisa Pratensis*, related to *"Succisa pratensis treatment induced chemotherapy sensibility in Glioblastoma cells"*.

## Succisa Pratensis molecules

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

- *docking* : contains all files related to molecular docking (input, output, configuration files etc...);
- *ligand* : contains the files for the tested ligands for docking, as they are available online;
- *PyMOL session* : contains PyMOL session save files to analyze docking poses;
- *paper figures* : contains the figures generated and used in the paper;
- *script* : contains the python scripts used to generate some graphs.



<br/>

## Software

> Software used:

- Open Babel (http://openbabel.org/wiki/Main_Page)
- Autodock4 (https://autodock.scripps.edu/)
- PyMOL (https://www.pymol.org/)


<br/>

![workflow](https://github.com/FabioPirovano/succisa_pratensis_docking/blob/main/paper_figures/extra/ppt%20figures/docking_workflow_5.png)
