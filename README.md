# BIGSSS-CSS Lab Journal
A lab journal template for students of the BIGSSS-CSS summer school about segregation & polarisation (https://robfranken.github.io/bigssslabjournal/). To use this, follow the steps below.


## Preparation:

1. Make sure R and R-studio are installed;
2. Make sure the `rmarkdown`-package is installed in R-studio (with "install dependencies");
3. Make sure [GitHub Desktop](https://desktop.github.com) is installed and connected to your GitHub account.

## Forking the repository:

1. Fork the repository using the fork button in the top right hand corner of Github, to make a personal copy of this lab journal;
2. Under your repository name, navigate to 'Settings'; click on 'Pages' on the sidebar; select the main-branch as your publishing source, and serve from the 'docs'-folder.

You now have a personal copy of this repository on your account, which serves html files (aka Github pages) as a website.

## Inviting collaborators:
Navigate to 'Settings'; click on 'Collaborators', and invite JochemTolsma, robfranken and thomasfeliciani. After acceptance, we have access to you repository and can make contributions.

## Clone the repository:
1. Using GitHub desktop, clone the forked lab journal repository to your local path;
2. Specify that you want to use the forked repository for your own purposes. 

The forked repository at your local path contains all of the files you need. All you need for it to work is (the latest version of) R and R-studio installed.

## Journal your work:
1. Right-click on the repository name and click 'Open in Rstudio'; alternately, you can navigate to the repository at your local path and open the 'bigssslabjournal.Rproj' file. This should automatically open R-studio, and your current working environment will be inside this project.
2. Inside R-studio you should see a files tab in the bottom right hand corner; 
3. Customize the 'index.Rmd' as you wish within R-studio, to make it your own;
4. Make sure to install the `remotes` and `klippy` packages. Commands are included in the index.Rmd.
5. Journal your work using .Rmd-files.

You can keep your personal notes and working scripts a separate folder. Make sure to include an underscore in the folder name (e.g., _test). Scripts contained in this folder will not be compiled.

## Hosting on Github:
1. Recompile the lab journal website using the build function in the top right hand corner;
2. Commit your changes and push them to GitHub using GitHub Desktop.
3. Your personal lab journal website will be published at: https://{USERNAME}.github.io/bigssslabjournal/
