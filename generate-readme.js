const { Octokit } = require("@octokit/rest");
const fs = require('fs');
require('dotenv').config();

const octokit = new Octokit({
  auth: process.env.UPDATE_README,
});

async function getRepoStats() {
  const repo = 'WGU_MSDA_Portfolio';  // Your repository name
  const owner = 'tysonbiegler';  // Your GitHub username

  const { data } = await octokit.repos.listLanguages({
    owner,
    repo,
  });

  return data;
}

async function updateReadme() {
  const stats = await getRepoStats();
  const readmeContent = `
# WGU MSDA Portfolio
## Tyson Biegler

**\`Data Analyst\`** **\`Grad Student\`**
<br>
<br>
I began learning about data analytics in 2020. I completed several certification programs ranging from the [Google data analytics professional certificate](https://www.coursera.org/account/accomplishments/professional-cert/PJYAPL3D7B2R) to [CS50](https://certificates.cs50.io/f6af04be-d727-4f4a-9236-4ef3e0989a83.pdf?size=letter) from Harvard University. I began my masters program in data analytics through Western governors university in May 2024 and this is my portfolio of projects from that program. 


<p><a href="https://www.linkedin.com/in/tysonbiegler/">
   <img src="https://img.shields.io/badge/linkedin-%230077B5.svg?&style=for-the-badge&logo=linkedin&logoColor=white" /></a>
</p>
---

### 🖥️ Languages and Tools
<img align="left" alt="Git" width="30px" style="padding-right:10px;" src="https://cdn.jsdelivr.net/gh/devicons/devicon/icons/git/git-original.svg" />
<img align="left" alt="HTML" width="30px" style="padding-right:10px;" src="https://cdn.jsdelivr.net/gh/devicons/devicon/icons/html5/html5-plain.svg" />
<img align="left" alt="CSS" width="30px" style="padding-right:10px;" src="https://cdn.jsdelivr.net/gh/devicons/devicon/icons/css3/css3-plain.svg" />
<img align="left" alt="R" width="30px" style="padding-right:10px;" src="https://www.r-project.org/logo/Rlogo.svg" />
<img align="left" alt="R Studio" width="30px" style="padding-right:10px;" src="https://th.bing.com/th/id/R.bef4329fc09a8e2ec3d33e50e2ce669e?rik=p9uAp6ZFXw06lg&pid=ImgRaw&r=0" />
<img align="left" alt="Python" width="30px" style="padding-right:10px;" src="https://cdn.jsdelivr.net/gh/devicons/devicon/icons/python/python-plain.svg" />
<img align="left" alt="Jupyter Notebooks" width="30px" style="padding-right:10px;" src="https://logos-download.com/wp-content/uploads/2021/01/Jupyter_Logo.png" />
<img align="left" alt="PostgreSQL" width="30px" style="padding-right:10px;" src="https://th.bing.com/th/id/R.c70f2deca63a20f47ba6dbc5e9f22af4?rik=DxXsa94FRFwm7w&pid=ImgRaw&r=0" />
<img align="left" alt="Tableau" width="30px" style="padding-right:10px;" src="https://pnghq.com/wp-content/uploads/tableau-full-logo-transparent-png-85396-768x432.png" />

<br />

#

### 🎓 My Course Assessments

<!-- BEGIN COURSE-->
- [Orientation - ORA1](Orientation%20-%20ORA1)
- [The Data Analytics Journey - D204](The%20Data%20Analytics%20Journey%20-%20D204)
- [Data Acquisition - D205](Data%20Acquisition%20-%20D205)
- [Data Cleaning - D206](Data%20Cleaning%20-%20D206)
- [Exploratory Data Analysis - D207](Exploratory%20Data%20Analysis%20-%20D207)
- [Predictive Modeling - D208](Predictive%20Modeling%20-%20D208) *Coming Soon*
- [Data Mining 1 - D209](Data%20Mining%201%20-%20D209) *Coming Soon*
- [Representation and Reporting - D210](Representation%20and%20Reporting%20-%20D210) *Coming Soon*
- [Advanced Data Acquisition - D211](Advanced%20Data%20Acquisition%20-%20D211) *Coming Soon*
- [Data Mining 2 - D212](Data%20Mining%202%20-%20D212) *Coming Soon*
- [Advanced Data Analytics - D213](Advanced%20Data%20Analytics%20-%20D213) *Coming Soon*
- [Data Analytics Graduate Capstone - D214](Data%20Analytics%20Graduate%20Capstone%20-%20D214) *Coming Soon*
<!-- END COURSES -->

#

### 📊 My Github Stats

<picture>
  <source
    srcset="https://github-readme-stats.vercel.app/api?username=tysonbiegler&show_icons=true&theme=dracula"
    media="(prefers-color-scheme: dark)"
  />
  <source
    srcset="https://github-readme-stats.vercel.app/api?username=tysonbiegler&show_icons=true"
    media="(prefers-color-scheme: light), (prefers-color
