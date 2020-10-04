# Source sidebar_tab
source("sidebar_tabs/data_import.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/descriptive_stats.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/stats_tests.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/dist_ad.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/graf.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/term_e_cond.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/local_run_s_tab.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/gama_family_s_tab.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/normal_family_s_tab.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/extreme_family_s_tab.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/reference_s_tab.R", encoding = "UTF-8", local = T)
source("sidebar_tabs/bug_report.R", encoding = "UTF-8", local = T)

# Page ui
dashboardPage(
  skin = "black",
  dashboardHeader(title = "FAM", titleWidth = 240),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar_p1",
      menuItem(
        "Data Import",
        tabName = "data_import",
        icon = icon("upload", lib = "font-awesome")
      )
    ),
    sidebarMenuOutput("menu"),
    sidebarMenu(
      id = "sidebar_p2",
      menuItem(
        "Terms & Conditions",
        tabName = "term_cond",
        icon = icon(name = "info-circle",class = "fas" ,lib = "font-awesome")
      ),
      
      menuItem(
        "Documentation",
        tabName = "info",
        icon = icon(name = "book-open",class = "fas" ,lib = "font-awesome"),
        menuSubItem(text = "Run Locally",
                    icon = icon("r-project",class = "fab" ,lib = "font-awesome"),
                    tabName = "run_local"),

                    menuSubItem(text = "Gamma Family",
                    icon = icon("plus",class = "fas" ,lib = "font-awesome") ,
                    tabName = "gamma_family"),
        menuSubItem(text = "Normal Family",
                    icon = icon("plus",class = "fas" ,lib = "font-awesome") ,
                    tabName = "normal_family"),
        menuSubItem(text = "Extreme Value Family",
                    icon = icon("plus",class = "fas" ,lib = "font-awesome") ,
                    tabName = "extreme_family"),
        menuSubItem(text = "Reference",
                    icon = icon("sticky-note",class = "far" ,lib = "font-awesome") ,
                    tabName = "reference")
      ),
      menuItem(
        "Found a Error/Bug",
        tabName = "bug_reports",
        icon = icon(name = "bug",class = "fas" ,lib = "font-awesome")
      )
      
      ),
    
    width = 240
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "custom.css")
    ),
    tabItems(
      tabItem("info", fluidRow()),
      data.import,
      term.cond,
      bug.reports.tab,
      desc.stats,
      stats.tests,
      dist.ad,
      plot.data.tab,
      run.local.s.tab,
      gamma.s.tab,
      normal.s.tab,
      extreme.s.tab,
      reference.s.tab
      
    )
  )
)