# CORAL tree shiny operator

##### Description

The `coral_shiny_v2_operator` is the Shiny version of the CORAL tree visualization. 
The CORAL tree can encode data in three features (node color, node size, and branch color), 
allows three modes of kinome visualization (the traditional kinome tree as well as 
radial and dynamic-force networks) and generates high-resolution scalable vector 
graphic files suitable for publication without the need for refinement in graphic 
editing software. 

##### Usage

Input projection|.
---|---
`y-axis`        | the kinase score 
`column`        | uniprot IDs of the kinases
`colors`        | the variables which define the node colors (can be multiple)
`labels`        | the variables which define the node size (can be multiple)

Output relations|.
---|---
CORAL tree visualization embedded in a Shiny application.

##### Screenshots
![Example screenshot](/static/screenshot.PNG?raw=true "Example of application")

##### Details

Coral is a user-friendly interactive web application for visualizing both 
quantitative and qualitative data. Unlike previous tools, Coral can encode data 
in three features (node color, node size, and branch color), allows three modes 
of kinome visualization (the traditional kinome tree as well as radial and 
dynamic-force networks) and generates high-resolution scalable vector graphic files 
suitable for publication without the need for refinement in graphic editing software. Due to its user-friendly, interactive, and highly customizable design, Coral is broadly applicable to high-throughput studies of the human kinome.

##### See Also

[original CORAL repository](https://github.com/dphansti/CORAL)
