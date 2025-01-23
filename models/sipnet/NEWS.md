# PEcAn.SIPNET 1.8.0.9000

## License change
* PEcAn.SIPNET is now distributed under the BSD three-clause license instead of the NCSA Open Source license.

## Fixed

* `write.config.SIPNET()` now respects PFTs defined in `settings$run$site$site.pft` when setting PFT-specific initial LAI (it previously only looked in a CSV file declared at `settings$run$inputs$pft.site$path`)
* The generated Sipnet run script (job.sh) now works correctly, including across machines, when met/input/output files are specified relative to the working directory (#3418). Absolute paths continue to work as always.

# PEcAn.SIPNET 1.8.0

* Support for all Sipnet variables in read_restart and write_restart, for integration with state data assimilation workflows

# PEcAn.SIPNET 1.7.1

* All changes in 1.7.1 and earlier were recorded in a single file for all of the PEcAn packages; please see 
https://github.com/PecanProject/pecan/blob/v1.7.1/CHANGELOG.md for details.
