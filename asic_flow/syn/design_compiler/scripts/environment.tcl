#-------------------------
# Library setting
#-------------------------
set design <TOP_MODULE_NAME>
set target_library    [list]
set link_library      "*"
set synthetic_library [<synopsys_synthetic.db>]
set target_library    [<target_library.db>]
set link_library      [<link_libraru.db]
set link_library      [concat $link_library $target_library $synthetic_library ]
mkdir reports output


