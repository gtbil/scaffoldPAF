# scaffoldPAF
This package is in development. It is intended to be a simpler replacement for reference-based scaffolding using RagTag.

Install with:

```r
remotes::install_github("gtbil/scaffoldPAF")
```

You need to provide `.paf` files from `minimap2`. I generated them like this:

```bash
minimap2 -L -c --MD --eqx --secondary=no -a -o ./pafs_asm20/${BASENAME}.sam -t45 Coker312_asm20.mmi ${PREFIX}
samtools view --bam ./pafs_asm20/${BASENAME}.sam | samtools sort -o ./pafs_asm20/${BASENAME}.bam --write-index -
samtools view --with-header ./pafs_asm20/${BASENAME}.sam | paftools.js sam2paf - > ./pafs_asm20/${BASENAME}.paf
```
