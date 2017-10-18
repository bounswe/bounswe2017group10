





<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
  <link rel="dns-prefetch" href="https://assets-cdn.github.com">
  <link rel="dns-prefetch" href="https://avatars0.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars1.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars2.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars3.githubusercontent.com">
  <link rel="dns-prefetch" href="https://github-cloud.s3.amazonaws.com">
  <link rel="dns-prefetch" href="https://user-images.githubusercontent.com/">



  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/frameworks-d7137690e30123bade38abb082ac79f36cc7a105ff92e602405f53b725465cab.css" media="all" rel="stylesheet" />
  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/github-ca05371259dcac7e46d20df340b58a39d7c675dff23c67eeef701a9532c23763.css" media="all" rel="stylesheet" />
  
  
  <link crossorigin="anonymous" href="https://assets-cdn.github.com/assets/site-9628bfea02884cc06cee3aa24062d81f93e06de09ed5bac5557deb155500e539.css" media="all" rel="stylesheet" />
  

  <meta name="viewport" content="width=device-width">
  
  <title>hs-webdriver/readme-example-beginner.md at master · kallisti-dev/hs-webdriver · GitHub</title>
  <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
  <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
  <meta property="fb:app_id" content="1401488693436528">

    
    <meta content="https://avatars3.githubusercontent.com/u/1530244?s=400&amp;v=4" property="og:image" /><meta content="GitHub" property="og:site_name" /><meta content="object" property="og:type" /><meta content="kallisti-dev/hs-webdriver" property="og:title" /><meta content="https://github.com/kallisti-dev/hs-webdriver" property="og:url" /><meta content="hs-webdriver - A Haskell client for the Selenium WebDriver protocol." property="og:description" />

  <link rel="assets" href="https://assets-cdn.github.com/">
  
  <meta name="pjax-timeout" content="1000">
  
  <meta name="request-id" content="CF20:38F8:51CB2E7:7D89D9F:59E72DFB" data-pjax-transient>
  

  <meta name="selected-link" value="repo_source" data-pjax-transient>

  <meta name="google-site-verification" content="KT5gs8h0wvaagLKAVWq8bbeNwnZZK1r1XQysX3xurLU">
<meta name="google-site-verification" content="ZzhVyEFwb7w3e0-uOTltm8Jsck2F5StVihD0exw2fsA">
    <meta name="google-analytics" content="UA-3769691-2">

<meta content="collector.githubapp.com" name="octolytics-host" /><meta content="github" name="octolytics-app-id" /><meta content="https://collector.githubapp.com/github-external/browser_event" name="octolytics-event-url" /><meta content="CF20:38F8:51CB2E7:7D89D9F:59E72DFB" name="octolytics-dimension-request_id" /><meta content="iad" name="octolytics-dimension-region_edge" /><meta content="iad" name="octolytics-dimension-region_render" />
<meta content="/&lt;user-name&gt;/&lt;repo-name&gt;/blob/show" data-pjax-transient="true" name="analytics-location" />




  <meta class="js-ga-set" name="dimension1" content="Logged Out">


  

      <meta name="hostname" content="github.com">
  <meta name="user-login" content="">

      <meta name="expected-hostname" content="github.com">
    <meta name="js-proxy-site-detection-payload" content="MTJlNjcxMTRlNDU0NTI5OTMxOGZmNDM4NWM0YzE4NGY0M2RlN2Q2ZTQwMGViNjAxNzA0YTljNGY4OWVkN2I1Mnx7InJlbW90ZV9hZGRyZXNzIjoiNzguMTg4LjEwMS4xMTgiLCJyZXF1ZXN0X2lkIjoiQ0YyMDozOEY4OjUxQ0IyRTc6N0Q4OUQ5Rjo1OUU3MkRGQiIsInRpbWVzdGFtcCI6MTUwODMyMjgxMSwiaG9zdCI6ImdpdGh1Yi5jb20ifQ==">


  <meta name="html-safe-nonce" content="766a1c92cd77c69b16c11e7ded80afe5fc8e600c">

  <meta http-equiv="x-pjax-version" content="3ad95ecc7a8a96c4eb6a06993a0a0cbd">
  

      <link href="https://github.com/kallisti-dev/hs-webdriver/commits/master.atom" rel="alternate" title="Recent Commits to hs-webdriver:master" type="application/atom+xml">

  <meta name="description" content="hs-webdriver - A Haskell client for the Selenium WebDriver protocol.">
  <meta name="go-import" content="github.com/kallisti-dev/hs-webdriver git https://github.com/kallisti-dev/hs-webdriver.git">

  <meta content="1530244" name="octolytics-dimension-user_id" /><meta content="kallisti-dev" name="octolytics-dimension-user_login" /><meta content="3700080" name="octolytics-dimension-repository_id" /><meta content="kallisti-dev/hs-webdriver" name="octolytics-dimension-repository_nwo" /><meta content="true" name="octolytics-dimension-repository_public" /><meta content="false" name="octolytics-dimension-repository_is_fork" /><meta content="3700080" name="octolytics-dimension-repository_network_root_id" /><meta content="kallisti-dev/hs-webdriver" name="octolytics-dimension-repository_network_root_nwo" /><meta content="false" name="octolytics-dimension-repository_explore_github_marketplace_ci_cta_shown" />


    <link rel="canonical" href="https://github.com/kallisti-dev/hs-webdriver/blob/master/examples/readme-example-beginner.md" data-pjax-transient>


  <meta name="browser-stats-url" content="https://api.github.com/_private/browser/stats">

  <meta name="browser-errors-url" content="https://api.github.com/_private/browser/errors">

  <link rel="mask-icon" href="https://assets-cdn.github.com/pinned-octocat.svg" color="#000000">
  <link rel="icon" type="image/x-icon" href="https://assets-cdn.github.com/favicon.ico">

<meta name="theme-color" content="#1e2327">



  </head>

  <body class="logged-out env-production page-blob">
    

  <div class="position-relative js-header-wrapper ">
    <a href="#start-of-content" tabindex="1" class="px-2 py-4 show-on-focus js-skip-to-content">Skip to content</a>
    <div id="js-pjax-loader-bar" class="pjax-loader-bar"><div class="progress"></div></div>

    
    
    



        <header class="Header header-logged-out  position-relative f4 py-3" role="banner">
  <div class="container-lg d-flex px-3">
    <div class="d-flex flex-justify-between flex-items-center">
      <a class="header-logo-invertocat my-0" href="https://github.com/" aria-label="Homepage" data-ga-click="(Logged out) Header, go to homepage, icon:logo-wordmark">
        <svg aria-hidden="true" class="octicon octicon-mark-github" height="32" version="1.1" viewBox="0 0 16 16" width="32"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"/></svg>
      </a>

    </div>

    <div class="HeaderMenu HeaderMenu--bright d-flex flex-justify-between flex-auto">
        <nav class="mt-0">
          <ul class="d-flex list-style-none">
              <li class="ml-2">
                <a href="/features" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:features" data-selected-links="/features /features/project-management /features/code-review /features/project-management /features/integrations /features">
                  Features
</a>              </li>
              <li class="ml-4">
                <a href="/business" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:business" data-selected-links="/business /business/security /business/customers /business">
                  Business
</a>              </li>

              <li class="ml-4">
                <a href="/explore" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:explore" data-selected-links="/explore /trending /trending/developers /integrations /integrations/feature/code /integrations/feature/collaborate /integrations/feature/ship showcases showcases_search showcases_landing /explore">
                  Explore
</a>              </li>

              <li class="ml-4">
                    <a href="/marketplace" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:marketplace" data-selected-links=" /marketplace">
                      Marketplace
</a>              </li>
              <li class="ml-4">
                <a href="/pricing" class="js-selected-navigation-item HeaderNavlink px-0 py-2 m-0" data-ga-click="Header, click, Nav menu - item:pricing" data-selected-links="/pricing /pricing/developer /pricing/team /pricing/business-hosted /pricing/business-enterprise /pricing">
                  Pricing
</a>              </li>
          </ul>
        </nav>

      <div class="d-flex">
          <div class="d-lg-flex flex-items-center mr-3">
            <div class="header-search scoped-search site-scoped-search js-site-search" role="search">
  <!-- '"` --><!-- </textarea></xmp> --></option></form><form accept-charset="UTF-8" action="/kallisti-dev/hs-webdriver/search" class="js-site-search-form" data-scoped-search-url="/kallisti-dev/hs-webdriver/search" data-unscoped-search-url="/search" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
    <label class="form-control header-search-wrapper js-chromeless-input-container">
        <a href="/kallisti-dev/hs-webdriver/blob/master/examples/readme-example-beginner.md" class="header-search-scope no-underline">This repository</a>
      <input type="text"
        class="form-control header-search-input js-site-search-focus js-site-search-field is-clearable"
        data-hotkey="s"
        name="q"
        value=""
        placeholder="Search"
        aria-label="Search this repository"
        data-unscoped-placeholder="Search GitHub"
        data-scoped-placeholder="Search"
        autocapitalize="off">
        <input type="hidden" class="js-site-search-type-field" name="type" >
    </label>
</form></div>

          </div>

        <span class="d-inline-block">
            <div class="HeaderNavlink px-0 py-2 m-0">
              <a class="text-bold text-white no-underline" href="/login?return_to=%2Fkallisti-dev%2Fhs-webdriver%2Fblob%2Fmaster%2Fexamples%2Freadme-example-beginner.md" data-ga-click="(Logged out) Header, clicked Sign in, text:sign-in">Sign in</a>
                <span class="text-gray">or</span>
                <a class="text-bold text-white no-underline" href="/join?source=header-repo" data-ga-click="(Logged out) Header, clicked Sign up, text:sign-up">Sign up</a>
            </div>
        </span>
      </div>
    </div>
  </div>
</header>


  </div>

  <div id="start-of-content" class="show-on-focus"></div>

    <div id="js-flash-container">
</div>



  <div role="main">
        <div itemscope itemtype="http://schema.org/SoftwareSourceCode">
    <div id="js-repo-pjax-container" data-pjax-container>
      





    <div class="pagehead repohead instapaper_ignore readability-menu experiment-repo-nav ">
      <div class="repohead-details-container clearfix container ">

        <ul class="pagehead-actions">
  <li>
      <a href="/login?return_to=%2Fkallisti-dev%2Fhs-webdriver"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to watch a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-eye" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.06 2C3 2 0 8 0 8s3 6 8.06 6C13 14 16 8 16 8s-3-6-7.94-6zM8 12c-2.2 0-4-1.78-4-4 0-2.2 1.8-4 4-4 2.22 0 4 1.8 4 4 0 2.22-1.78 4-4 4zm2-4c0 1.11-.89 2-2 2-1.11 0-2-.89-2-2 0-1.11.89-2 2-2 1.11 0 2 .89 2 2z"/></svg>
    Watch
  </a>
  <a class="social-count" href="/kallisti-dev/hs-webdriver/watchers"
     aria-label="11 users are watching this repository">
    11
  </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fkallisti-dev%2Fhs-webdriver"
    class="btn btn-sm btn-with-count tooltipped tooltipped-n"
    aria-label="You must be signed in to star a repository" rel="nofollow">
    <svg aria-hidden="true" class="octicon octicon-star" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M14 6l-4.9-.64L7 1 4.9 5.36 0 6l3.6 3.26L2.67 14 7 11.67 11.33 14l-.93-4.74z"/></svg>
    Star
  </a>

    <a class="social-count js-social-count" href="/kallisti-dev/hs-webdriver/stargazers"
      aria-label="122 users starred this repository">
      122
    </a>

  </li>

  <li>
      <a href="/login?return_to=%2Fkallisti-dev%2Fhs-webdriver"
        class="btn btn-sm btn-with-count tooltipped tooltipped-n"
        aria-label="You must be signed in to fork a repository" rel="nofollow">
        <svg aria-hidden="true" class="octicon octicon-repo-forked" height="16" version="1.1" viewBox="0 0 10 16" width="10"><path fill-rule="evenodd" d="M8 1a1.993 1.993 0 0 0-1 3.72V6L5 8 3 6V4.72A1.993 1.993 0 0 0 2 1a1.993 1.993 0 0 0-1 3.72V6.5l3 3v1.78A1.993 1.993 0 0 0 5 15a1.993 1.993 0 0 0 1-3.72V9.5l3-3V4.72A1.993 1.993 0 0 0 8 1zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3 10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3-10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z"/></svg>
        Fork
      </a>

    <a href="/kallisti-dev/hs-webdriver/network" class="social-count"
       aria-label="55 users forked this repository">
      55
    </a>
  </li>
</ul>

        <h1 class="public ">
  <svg aria-hidden="true" class="octicon octicon-repo" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M4 9H3V8h1v1zm0-3H3v1h1V6zm0-2H3v1h1V4zm0-2H3v1h1V2zm8-1v12c0 .55-.45 1-1 1H6v2l-1.5-1.5L3 16v-2H1c-.55 0-1-.45-1-1V1c0-.55.45-1 1-1h10c.55 0 1 .45 1 1zm-1 10H1v2h2v-1h3v1h5v-2zm0-10H2v9h9V1z"/></svg>
  <span class="author" itemprop="author"><a href="/kallisti-dev" class="url fn" rel="author">kallisti-dev</a></span><!--
--><span class="path-divider">/</span><!--
--><strong itemprop="name"><a href="/kallisti-dev/hs-webdriver" data-pjax="#js-repo-pjax-container">hs-webdriver</a></strong>

</h1>

      </div>
      
<nav class="reponav js-repo-nav js-sidenav-container-pjax container"
     itemscope
     itemtype="http://schema.org/BreadcrumbList"
     role="navigation"
     data-pjax="#js-repo-pjax-container">

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/kallisti-dev/hs-webdriver" class="js-selected-navigation-item selected reponav-item" data-hotkey="g c" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches repo_packages /kallisti-dev/hs-webdriver" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-code" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M9.5 3L8 4.5 11.5 8 8 11.5 9.5 13 14 8 9.5 3zm-5 0L0 8l4.5 5L6 11.5 2.5 8 6 4.5 4.5 3z"/></svg>
      <span itemprop="name">Code</span>
      <meta itemprop="position" content="1">
</a>  </span>

    <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
      <a href="/kallisti-dev/hs-webdriver/issues" class="js-selected-navigation-item reponav-item" data-hotkey="g i" data-selected-links="repo_issues repo_labels repo_milestones /kallisti-dev/hs-webdriver/issues" itemprop="url">
        <svg aria-hidden="true" class="octicon octicon-issue-opened" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z"/></svg>
        <span itemprop="name">Issues</span>
        <span class="Counter">12</span>
        <meta itemprop="position" content="2">
</a>    </span>

  <span itemscope itemtype="http://schema.org/ListItem" itemprop="itemListElement">
    <a href="/kallisti-dev/hs-webdriver/pulls" class="js-selected-navigation-item reponav-item" data-hotkey="g p" data-selected-links="repo_pulls /kallisti-dev/hs-webdriver/pulls" itemprop="url">
      <svg aria-hidden="true" class="octicon octicon-git-pull-request" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M11 11.28V5c-.03-.78-.34-1.47-.94-2.06C9.46 2.35 8.78 2.03 8 2H7V0L4 3l3 3V4h1c.27.02.48.11.69.31.21.2.3.42.31.69v6.28A1.993 1.993 0 0 0 10 15a1.993 1.993 0 0 0 1-3.72zm-1 2.92c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zM4 3c0-1.11-.89-2-2-2a1.993 1.993 0 0 0-1 3.72v6.56A1.993 1.993 0 0 0 2 15a1.993 1.993 0 0 0 1-3.72V4.72c.59-.34 1-.98 1-1.72zm-.8 10c0 .66-.55 1.2-1.2 1.2-.65 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z"/></svg>
      <span itemprop="name">Pull requests</span>
      <span class="Counter">5</span>
      <meta itemprop="position" content="3">
</a>  </span>

    <a href="/kallisti-dev/hs-webdriver/projects" class="js-selected-navigation-item reponav-item" data-hotkey="g b" data-selected-links="repo_projects new_repo_project repo_project /kallisti-dev/hs-webdriver/projects">
      <svg aria-hidden="true" class="octicon octicon-project" height="16" version="1.1" viewBox="0 0 15 16" width="15"><path fill-rule="evenodd" d="M10 12h3V2h-3v10zm-4-2h3V2H6v8zm-4 4h3V2H2v12zm-1 1h13V1H1v14zM14 0H1a1 1 0 0 0-1 1v14a1 1 0 0 0 1 1h13a1 1 0 0 0 1-1V1a1 1 0 0 0-1-1z"/></svg>
      Projects
      <span class="Counter" >0</span>
</a>


  <a href="/kallisti-dev/hs-webdriver/pulse" class="js-selected-navigation-item reponav-item" data-selected-links="repo_graphs repo_contributors dependency_graph pulse /kallisti-dev/hs-webdriver/pulse">
    <svg aria-hidden="true" class="octicon octicon-graph" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M16 14v1H0V0h1v14h15zM5 13H3V8h2v5zm4 0H7V3h2v10zm4 0h-2V6h2v7z"/></svg>
    Insights
</a>

</nav>


    </div>

<div class="container new-discussion-timeline experiment-repo-nav">
  <div class="repository-content">

    
  <a href="/kallisti-dev/hs-webdriver/blob/0ec102faea114855b5e52f68585e87bde010804a/examples/readme-example-beginner.md" class="d-none js-permalink-shortcut" data-hotkey="y">Permalink</a>

  <!-- blob contrib key: blob_contributors:v21:d159f7ff2e426512f0a227e4881ff7c7 -->

  <div class="file-navigation js-zeroclipboard-container">
    
<div class="select-menu branch-select-menu js-menu-container js-select-menu float-left">
  <button class=" btn btn-sm select-menu-button js-menu-target css-truncate" data-hotkey="w"
    
    type="button" aria-label="Switch branches or tags" aria-expanded="false" aria-haspopup="true">
      <i>Branch:</i>
      <span class="js-select-button css-truncate-target">master</span>
  </button>

  <div class="select-menu-modal-holder js-menu-content js-navigation-container" data-pjax>

    <div class="select-menu-modal">
      <div class="select-menu-header">
        <svg aria-label="Close" class="octicon octicon-x js-menu-close" height="16" role="img" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
        <span class="select-menu-title">Switch branches/tags</span>
      </div>

      <div class="select-menu-filters">
        <div class="select-menu-text-filter">
          <input type="text" aria-label="Filter branches/tags" id="context-commitish-filter-field" class="form-control js-filterable-field js-navigation-enable" placeholder="Filter branches/tags">
        </div>
        <div class="select-menu-tabs">
          <ul>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="branches" data-filter-placeholder="Filter branches/tags" class="js-select-menu-tab" role="tab">Branches</a>
            </li>
            <li class="select-menu-tab">
              <a href="#" data-tab-filter="tags" data-filter-placeholder="Find a tag…" class="js-select-menu-tab" role="tab">Tags</a>
            </li>
          </ul>
        </div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="branches" role="menu">

        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/kallisti-dev/hs-webdriver/blob/browser-gadt/examples/readme-example-beginner.md"
               data-name="browser-gadt"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                browser-gadt
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open selected"
               href="/kallisti-dev/hs-webdriver/blob/master/examples/readme-example-beginner.md"
               data-name="master"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                master
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
               href="/kallisti-dev/hs-webdriver/blob/vinyl-capabilities/examples/readme-example-beginner.md"
               data-name="vinyl-capabilities"
               data-skip-pjax="true"
               rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target js-select-menu-filter-text">
                vinyl-capabilities
              </span>
            </a>
        </div>

          <div class="select-menu-no-results">Nothing to show</div>
      </div>

      <div class="select-menu-list select-menu-tab-bucket js-select-menu-tab-bucket" data-tab-filter="tags">
        <div data-filterable-for="context-commitish-filter-field" data-filterable-type="substring">


            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.5/examples/readme-example-beginner.md"
              data-name="0.8.5"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.5">
                0.8.5
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.4/examples/readme-example-beginner.md"
              data-name="0.8.4"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.4">
                0.8.4
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.3/examples/readme-example-beginner.md"
              data-name="0.8.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.3">
                0.8.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.2/examples/readme-example-beginner.md"
              data-name="0.8.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.2">
                0.8.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.1/examples/readme-example-beginner.md"
              data-name="0.8.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.1">
                0.8.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.0.4/examples/readme-example-beginner.md"
              data-name="0.8.0.4"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.0.4">
                0.8.0.4
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.0.3/examples/readme-example-beginner.md"
              data-name="0.8.0.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.0.3">
                0.8.0.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.0.2/examples/readme-example-beginner.md"
              data-name="0.8.0.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.0.2">
                0.8.0.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8.0.1/examples/readme-example-beginner.md"
              data-name="0.8.0.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8.0.1">
                0.8.0.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.8/examples/readme-example-beginner.md"
              data-name="0.8"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.8">
                0.8
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.7/examples/readme-example-beginner.md"
              data-name="0.7"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.7">
                0.7
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.3.1/examples/readme-example-beginner.md"
              data-name="0.6.3.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.3.1">
                0.6.3.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.3/examples/readme-example-beginner.md"
              data-name="0.6.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.3">
                0.6.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.2.1/examples/readme-example-beginner.md"
              data-name="0.6.2.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.2.1">
                0.6.2.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.2/examples/readme-example-beginner.md"
              data-name="0.6.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.2">
                0.6.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.1/examples/readme-example-beginner.md"
              data-name="0.6.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.1">
                0.6.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.0.4/examples/readme-example-beginner.md"
              data-name="0.6.0.4"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.0.4">
                0.6.0.4
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.0.3/examples/readme-example-beginner.md"
              data-name="0.6.0.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.0.3">
                0.6.0.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6.0.2/examples/readme-example-beginner.md"
              data-name="0.6.0.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6.0.2">
                0.6.0.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.6/examples/readme-example-beginner.md"
              data-name="0.6"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.6">
                0.6
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5.3.3/examples/readme-example-beginner.md"
              data-name="0.5.3.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5.3.3">
                0.5.3.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5.3.2/examples/readme-example-beginner.md"
              data-name="0.5.3.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5.3.2">
                0.5.3.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5.3.1/examples/readme-example-beginner.md"
              data-name="0.5.3.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5.3.1">
                0.5.3.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5.3/examples/readme-example-beginner.md"
              data-name="0.5.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5.3">
                0.5.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5.2/examples/readme-example-beginner.md"
              data-name="0.5.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5.2">
                0.5.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5.1/examples/readme-example-beginner.md"
              data-name="0.5.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5.1">
                0.5.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5.0.1/examples/readme-example-beginner.md"
              data-name="0.5.0.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5.0.1">
                0.5.0.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.5/examples/readme-example-beginner.md"
              data-name="0.5"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.5">
                0.5
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.4/examples/readme-example-beginner.md"
              data-name="0.4"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.4">
                0.4
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.3.3/examples/readme-example-beginner.md"
              data-name="0.3.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.3.3">
                0.3.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.3.2.1/examples/readme-example-beginner.md"
              data-name="0.3.2.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.3.2.1">
                0.3.2.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.3.2/examples/readme-example-beginner.md"
              data-name="0.3.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.3.2">
                0.3.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.3.1/examples/readme-example-beginner.md"
              data-name="0.3.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.3.1">
                0.3.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.3.0.1/examples/readme-example-beginner.md"
              data-name="0.3.0.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.3.0.1">
                0.3.0.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.3/examples/readme-example-beginner.md"
              data-name="0.3"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.3">
                0.3
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.2/examples/readme-example-beginner.md"
              data-name="0.2"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.2">
                0.2
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.1/examples/readme-example-beginner.md"
              data-name="0.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.1">
                0.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0.0.1/examples/readme-example-beginner.md"
              data-name="0.0.1"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0.0.1">
                0.0.1
              </span>
            </a>
            <a class="select-menu-item js-navigation-item js-navigation-open "
              href="/kallisti-dev/hs-webdriver/tree/0/examples/readme-example-beginner.md"
              data-name="0"
              data-skip-pjax="true"
              rel="nofollow">
              <svg aria-hidden="true" class="octicon octicon-check select-menu-item-icon" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M12 5l-8 8-4-4 1.5-1.5L4 10l6.5-6.5z"/></svg>
              <span class="select-menu-item-text css-truncate-target" title="0">
                0
              </span>
            </a>
        </div>

        <div class="select-menu-no-results">Nothing to show</div>
      </div>

    </div>
  </div>
</div>

    <div class="BtnGroup float-right">
      <a href="/kallisti-dev/hs-webdriver/find/master"
            class="js-pjax-capture-input btn btn-sm BtnGroup-item"
            data-pjax
            data-hotkey="t">
        Find file
      </a>
      <button aria-label="Copy file path to clipboard" class="js-zeroclipboard btn btn-sm BtnGroup-item tooltipped tooltipped-s" data-copied-hint="Copied!" type="button">Copy path</button>
    </div>
    <div class="breadcrumb js-zeroclipboard-target">
      <span class="repo-root js-repo-root"><span class="js-path-segment"><a href="/kallisti-dev/hs-webdriver"><span>hs-webdriver</span></a></span></span><span class="separator">/</span><span class="js-path-segment"><a href="/kallisti-dev/hs-webdriver/tree/master/examples"><span>examples</span></a></span><span class="separator">/</span><strong class="final-path">readme-example-beginner.md</strong>
    </div>
  </div>


  
  <div class="commit-tease">
      <span class="float-right">
        <a class="commit-tease-sha" href="/kallisti-dev/hs-webdriver/commit/c3cd8bb8233b21bbe26480e85ea2be84ba5c31c2" data-pjax>
          c3cd8bb
        </a>
        <relative-time datetime="2016-08-31T11:24:57Z">Aug 31, 2016</relative-time>
      </span>
      <div>
        <img alt="@CristhianMotoche" class="avatar" height="20" src="https://avatars3.githubusercontent.com/u/8370088?s=40&amp;v=4" width="20" />
        <a href="/CristhianMotoche" class="user-mention" rel="contributor">CristhianMotoche</a>
          <a href="/kallisti-dev/hs-webdriver/commit/c3cd8bb8233b21bbe26480e85ea2be84ba5c31c2" class="message" data-pjax="true" title="Update links">Update links</a>
      </div>

    <div class="commit-tease-contributors">
      <button type="button" class="btn-link muted-link contributors-toggle" data-facebox="#blob_contributors_box">
        <strong>3</strong>
         contributors
      </button>
          <a class="avatar-link tooltipped tooltipped-s" aria-label="kallisti-dev" href="/kallisti-dev/hs-webdriver/commits/master/examples/readme-example-beginner.md?author=kallisti-dev"><img alt="@kallisti-dev" class="avatar" height="20" src="https://avatars2.githubusercontent.com/u/1530244?s=40&amp;v=4" width="20" /> </a>
    <a class="avatar-link tooltipped tooltipped-s" aria-label="CristhianMotoche" href="/kallisti-dev/hs-webdriver/commits/master/examples/readme-example-beginner.md?author=CristhianMotoche"><img alt="@CristhianMotoche" class="avatar" height="20" src="https://avatars3.githubusercontent.com/u/8370088?s=40&amp;v=4" width="20" /> </a>
    <a class="avatar-link tooltipped tooltipped-s" aria-label="beerendlauwers" href="/kallisti-dev/hs-webdriver/commits/master/examples/readme-example-beginner.md?author=beerendlauwers"><img alt="@beerendlauwers" class="avatar" height="20" src="https://avatars3.githubusercontent.com/u/3713919?s=40&amp;v=4" width="20" /> </a>


    </div>

    <div id="blob_contributors_box" style="display:none">
      <h2 class="facebox-header" data-facebox-id="facebox-header">Users who have contributed to this file</h2>
      <ul class="facebox-user-list" data-facebox-id="facebox-description">
          <li class="facebox-user-list-item">
            <img alt="@kallisti-dev" height="24" src="https://avatars3.githubusercontent.com/u/1530244?s=48&amp;v=4" width="24" />
            <a href="/kallisti-dev">kallisti-dev</a>
          </li>
          <li class="facebox-user-list-item">
            <img alt="@CristhianMotoche" height="24" src="https://avatars2.githubusercontent.com/u/8370088?s=48&amp;v=4" width="24" />
            <a href="/CristhianMotoche">CristhianMotoche</a>
          </li>
          <li class="facebox-user-list-item">
            <img alt="@beerendlauwers" height="24" src="https://avatars2.githubusercontent.com/u/3713919?s=48&amp;v=4" width="24" />
            <a href="/beerendlauwers">beerendlauwers</a>
          </li>
      </ul>
    </div>
  </div>


  <div class="file">
    <div class="file-header">
  <div class="file-actions">

    <div class="BtnGroup">
      <a href="/kallisti-dev/hs-webdriver/raw/master/examples/readme-example-beginner.md" class="btn btn-sm BtnGroup-item" id="raw-url">Raw</a>
        <a href="/kallisti-dev/hs-webdriver/blame/master/examples/readme-example-beginner.md" class="btn btn-sm js-update-url-with-hash BtnGroup-item" data-hotkey="b">Blame</a>
      <a href="/kallisti-dev/hs-webdriver/commits/master/examples/readme-example-beginner.md" class="btn btn-sm BtnGroup-item" rel="nofollow">History</a>
    </div>


        <button type="button" class="btn-octicon disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-pencil" height="16" version="1.1" viewBox="0 0 14 16" width="14"><path fill-rule="evenodd" d="M0 12v3h3l8-8-3-3-8 8zm3 2H1v-2h1v1h1v1zm10.3-9.3L12 6 9 3l1.3-1.3a.996.996 0 0 1 1.41 0l1.59 1.59c.39.39.39 1.02 0 1.41z"/></svg>
        </button>
        <button type="button" class="btn-octicon btn-octicon-danger disabled tooltipped tooltipped-nw"
          aria-label="You must be signed in to make or propose changes">
          <svg aria-hidden="true" class="octicon octicon-trashcan" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M11 2H9c0-.55-.45-1-1-1H5c-.55 0-1 .45-1 1H2c-.55 0-1 .45-1 1v1c0 .55.45 1 1 1v9c0 .55.45 1 1 1h7c.55 0 1-.45 1-1V5c.55 0 1-.45 1-1V3c0-.55-.45-1-1-1zm-1 12H3V5h1v8h1V5h1v8h1V5h1v8h1V5h1v9zm1-10H2V3h9v1z"/></svg>
        </button>
  </div>

  <div class="file-info">
      204 lines (144 sloc)
      <span class="file-info-divider"></span>
    10.4 KB
  </div>
</div>

    
  <div id="readme" class="readme blob instapaper_body">
    <article class="markdown-body entry-content" itemprop="text"><h1><a href="#installation-and-environment-setup" aria-hidden="true" class="anchor" id="user-content-installation-and-environment-setup"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Installation and Environment Setup</h1>
<p>This example assumes you already have hs-webdriver installed and already have a Selenium server running on your local
machine. If you haven't done these steps, refer to the <a href="/kallisti-dev/hs-webdriver/blob/master/README.md">README</a> for detailed installation/setup instructions.</p>
<p>What this example does not assume is any prior Haskell knowledge on your part. It is intended for anyone with basic
programming knowledge to understand. However, it cannot fully replace a good tutorial or guide on Haskell. For that,
I recommend reading <a href="http://learnyouahaskell.com/">Learn You a Haskell for Great Good</a>.</p>
<p>If you have any questions or need clarification about anything in this example, please send an email to the project
maintainer.</p>
<h1><a href="#boilerplate" aria-hidden="true" class="anchor" id="user-content-boilerplate"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Boilerplate</h1>
<p>Before you use the library, you'll need to add a bit of boilerplate to your Haskell code:</p>
<pre lang="{.haskell}"><code>{-# LANGUAGE OverloadedStrings #-}
</code></pre>
<p>Normally, Haskell string literals (such as <code>"Hello!"</code>) have the type <code>String</code>, which is a linked list of Unicode code points.
Adding this to the top of your program file instructs GHC to allow string literals to be overloaded to other string-like
types. Since hs-webdriver uses the very efficient <code>Text</code> type for Unicode strings, the OverloadedStrings extension makes
it significantly easier to write your code without having to convert all of your <code>String</code> values to <code>Text</code> ones. Using the
<a href="https://hackage.haskell.org/package/text">text package</a> for efficient Unicode string handling is considered standard
practice in "Real World Haskell™".</p>
<p>And, of course, you need an <code>import</code> declaration so Haskell knows how to find the library!</p>
<pre lang="{.haskell}"><code>import Test.WebDriver
</code></pre>
<h1><a href="#configuration" aria-hidden="true" class="anchor" id="user-content-configuration"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Configuration</h1>
<pre lang="{.haskell}"><code>firefoxConfig :: WDConfig
firefoxConfig = defaultConfig
</code></pre>
<p>Here we tell Haskell to define a very basic configuration for a WebDriver session, naming it <code>firefoxConfig</code>.
The <code>defaultConfig</code> here refers to a pre-defined data structure exported by the library, which tells webdriver
to use firefox as our browser and has a very sensible configuration for usage when the WebDriver server is on our local
machine.</p>
<p>Note: The first line, containing the <code>::</code>, is a type signature for our newly defined variable, but it can be omitted because
Haskell will automatically infer the types of values. Its only purpose here is to indicate to the reader that <code>firefoxConfig</code>
has the type<code>WDConfig</code></p>
<h2><a href="#changing-the-browser" aria-hidden="true" class="anchor" id="user-content-changing-the-browser"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Changing the Browser</h2>
<p>Now let's make a new configuration that uses Chrome instead of Firefox.</p>
<pre lang="{.haskell}"><code>chromeConfig = useBrowser chrome defaultConfig
</code></pre>
<p>The <code>useBrowser</code> function above overrides the browser of <code>defaultConfig</code> to use <code>chrome</code> instead.</p>
<h2><a href="#connecting-to-a-remote-server" aria-hidden="true" class="anchor" id="user-content-connecting-to-a-remote-server"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Connecting to a Remote Server</h2>
<p>Now for a more complex configuration. Let's say we want a session that connects to a remote server and runs the Opera
browser.</p>
<p><em>Note</em>: To use Google Chrome, you need to install Google's proprietary
<a href="https://sites.google.com/a/chromium.org/chromedriver/">ChromeDriver</a> in a directory where it can be recognized by Selenium
Server (see: <a href="https://github.com/SeleniumHQ/selenium/wiki/ChromeDriver">https://github.com/SeleniumHQ/selenium/wiki/ChromeDriver</a>). When using propriertary webdrivers such as chromedriver,
Selenium Server simply acts as a proxy server, automatically spawning a proprietary server process as needed. Because of
this extra installation step, we simply use the <code>firefoxConfig</code> for the remainder of the example.</p>
<pre lang="{.haskell}"><code>remoteConfig = useBrowser opera defaultConfig { wdHost = "secret-offshore-testing-facility.org"
                                              , wdPort = 666 }
</code></pre>
<p>The curly braces are part of Haskell's record update syntax. They take the record immediately to the left of
the opening brace (in this case <code>defaultConfig</code>) and update some of its fields (in this case <code>wdHost</code> and <code>wdPort</code>)
with new values.</p>
<p>Now a second remote server configuration:</p>
<pre lang="{.haskell}"><code>remoteConfig2 = chromeConfig { wdHost = "really-bad-network-connection.biz"
                             , wdPort = 1234
                             , wdHTTPRetryCount = 50 }
</code></pre>
<p>Here we use the <code>chromeConfig</code> that we defined previously and modify it with new options. The <code>wdHTTPRetryCount</code> option sets
the number of times we try to send a WebDriver command to an unresponsive remote end.</p>
<h1><a href="#hello-world" aria-hidden="true" class="anchor" id="user-content-hello-world"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Hello, World!</h1>
<p>Interacting with the browser is done via writing a sequence of commands that are executed in order. For those familiar with
so-called turtle languages, which are used to teach programming by typing commands that draw shapes on screen, you should
feel pretty comfortable with this sort of interface.</p>
<pre lang="{.haskell}"><code>main = runSession firefoxConfig $ do                      -- starts a WebDriver session with the given firefox config, then
                                                          -- runs the supplied commands

  openPage "http://google.com"                            -- tells the browser to open the URL http://google.com

  searchInput &lt;- findElem ( ByCSS "input[type='text']" )  -- asks the browser to find an element on the page with the given
                                                          -- CSS selector then stores the resulting element in the varialbe 
                                                          -- named`searchInput`
                                                          
  sendKeys "Hello, World!" searchInput                    -- type into the element, as though a user had issued the
                                                          -- keystrokes `Hello, World!`
                                                          
  submit searchInput                                      -- submit the input form (technically not required with Google
                                                          -- but included for example purposes)

  closeSession                                            -- finally, close the WebDriver session and its associated
                                                          -- browser process 
</code></pre>
<p>The code should be clear even without the comments. But the important things to note about the syntax are:</p>
<ul>
<li>
<p>The <code>do</code> notation is used to begin an indented block of commands. This is analogous to curly braces in C-based language.</p>
</li>
<li>
<p>The <code>&lt;-</code> notation binds the result of executing the command on the right to the variable on the left.
In order to do anything meaningful with the results of a command, we must first bind the result to a name in this way.</p>
</li>
<li>
<p>Parentheses are required for function parameters that are more than one word long. Instead of <code>f(x, g(y), z)</code> as in most
languages you instead write <code>f x (g y) z</code></p>
</li>
<li>
<p>The <code>$</code> operator is equivalent to normal function application. Because it has very low precedence, we can use it to avoid
parentheses when grouping function parameters. Without the $ in the example above,
we would have to wrap the entire do block in a set of parentheses. We could also choose to rewrite the findElem command as</p>
<div class="highlight highlight-source-haskell"><pre> 	findElem <span class="pl-k">$</span> <span class="pl-ent">ByCSS</span> <span class="pl-s"><span class="pl-pds">"</span>input[type='text']<span class="pl-pds">"</span></span></pre></div>
<p><em>More about <code>$</code></em>: Haskell beginners are often put off by <code>$</code>, because they assume it's a special syntax form. This leads
them to believe that Haskell must have a lot of unnecessary and convoluted syntax, but this is not true.
Because Haskell allows you to define custom infix operators, <code>$</code> can be easily defined in code:</p>
<div class="highlight highlight-source-haskell"><pre> 	f <span class="pl-k">$</span> x <span class="pl-k">=</span> f x
 	infxr <span class="pl-c1">1</span> <span class="pl-k">$</span>   <span class="pl-c"><span class="pl-c">--</span> makes $ right-associative with lowest possible precedence</span></pre></div>
<p>The full details of Haskell syntax are outside the scope of this introduction, but for more information this
<a href="https://www.fpcomplete.com/blog/2012/09/ten-things-you-should-know-about-haskell-syntax">FPComplete blog post</a>
provides a basic overview of Haskell syntax for beginners.</p>
</li>
</ul>
<h1><a href="#about-main" aria-hidden="true" class="anchor" id="user-content-about-main"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>About <code>main</code></h1>
<p>In a Haskell program, the special variable <code>main</code> refers to the entry point of execution. In order for our program to be
valid, <code>main</code> must have the following type</p>
<div class="highlight highlight-source-haskell"><pre>	<span class="pl-en">main</span> <span class="pl-k">::</span> <span class="pl-en"><span class="pl-c1">IO</span></span> <span class="pl-smi">a</span></pre></div>
<p>So, to run a test we need to define a valid <code>main</code>, but our do-block has the type <code>WD a</code>, which is the type of WebDriver
command sequences. In order to convert our <code>WD</code> to an <code>IO</code> we have to use the <code>runSession</code> function, which has this type</p>
<div class="highlight highlight-source-haskell"><pre>	<span class="pl-en">runSession</span> <span class="pl-k">::</span> <span class="pl-en">WDConfig</span> <span class="pl-k">-&gt;</span> <span class="pl-en">WD</span> <span class="pl-smi">a</span> <span class="pl-k">-&gt;</span> <span class="pl-en"><span class="pl-c1">IO</span></span> <span class="pl-smi">a</span></pre></div>
<p>This says that runSession takes a webdriver config (as described previously) and a list of webdriver commands, and converts
them into a sequence of IO actions that the GHC runtime can understand. In doing so, it also initialzes a new WebDriver
session with information from the supplied <code>WDConfig</code>.</p>
<p>#WebDriver commands</p>
<p>This example contains all of the basic elements of a simple WebDriver test, using a few simple and commonly used commands.
For complete documentation on all the other commands you can use, check out
<a href="https://hackage.haskell.org/package/webdriver-0.6.0.4/docs/Test-WebDriver-Commands.html">Test.WebDriver.Commands</a> on
Hackage.</p>
<h1><a href="#monads-and-other-advanced-library-concepts" aria-hidden="true" class="anchor" id="user-content-monads-and-other-advanced-library-concepts"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Monads and other advanced library concepts</h1>
<p>hs-webdriver implements a very simple EDSL (embedded domain-specific language) for users to write web automation scripts.
This EDSL is implemented using a state monad. The state monad maintains implicit information about the WebDriver session
between commands, so that individual commands only require you to specify the parameters relevant to the action they perform.
Ironically enough, in some ways this makes the Haskell bindings for WebDriver more imperative than the bindings used by
imperative languages.</p>
<p>A quick aside about monads: Monads are a core concept of Haskell programming. But it's important to realize <em>you do not need
to understand how monads work to use this library</em>, in much the same way that you do not need to understand a sorting
algorithm to sort data with it. In fact the choice of using a monadic interface was intended to make the interface easy to
understand, because it conforms naturally to the sequential imperative nature of WebDriver, which is essentially a list of
instructions for a browser to perform. I've even heard some anecdotes from developers at companies that chose to adopt
Haskell for web development, saying that their QA testers, with limited programming knowledge, found the Haskell bindings
easier to understand than the NodeJS WebDriver bindings.</p>
<h1><a href="#literate-haskell" aria-hidden="true" class="anchor" id="user-content-literate-haskell"><svg aria-hidden="true" class="octicon octicon-link" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg></a>Literate Haskell</h1>
<p>Using <a href="https://github.com/sol/markdown-unlit">markdown-unlit</a>, you can compile this example as Literate Haskell with GHC.
Just make sure to use the file extension <code>.lhs</code> before compiling.</p>
<div class="highlight highlight-source-shell"><pre>cabal update <span class="pl-k">&amp;&amp;</span> cabal install markdown-unlit
ghc -pgmL markdown-unlit examples/readme-example-beginner.lhs</pre></div>
<p>All example code is also part of the webdriver test suites, so a <code>cabal test</code> command will also run example code.</p>
</article>
  </div>

  </div>

  <button type="button" data-facebox="#jump-to-line" data-facebox-class="linejump" data-hotkey="l" class="d-none">Jump to Line</button>
  <div id="jump-to-line" style="display:none">
    <!-- '"` --><!-- </textarea></xmp> --></option></form><form accept-charset="UTF-8" action="" class="js-jump-to-line-form" method="get"><div style="margin:0;padding:0;display:inline"><input name="utf8" type="hidden" value="&#x2713;" /></div>
      <input class="form-control linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" aria-label="Jump to line" autofocus>
      <button type="submit" class="btn">Go</button>
</form>  </div>

  </div>
  <div class="modal-backdrop js-touch-events"></div>
</div>

    </div>
  </div>

  </div>

      
<div class="footer container-lg px-3" role="contentinfo">
  <div class="position-relative d-flex flex-justify-between py-6 mt-6 f6 text-gray border-top border-gray-light ">
    <ul class="list-style-none d-flex flex-wrap ">
      <li class="mr-3">&copy; 2017 <span title="0.12196s from unicorn-268052821-42hrk">GitHub</span>, Inc.</li>
        <li class="mr-3"><a href="https://github.com/site/terms" data-ga-click="Footer, go to terms, text:terms">Terms</a></li>
        <li class="mr-3"><a href="https://github.com/site/privacy" data-ga-click="Footer, go to privacy, text:privacy">Privacy</a></li>
        <li class="mr-3"><a href="https://github.com/security" data-ga-click="Footer, go to security, text:security">Security</a></li>
        <li class="mr-3"><a href="https://status.github.com/" data-ga-click="Footer, go to status, text:status">Status</a></li>
        <li><a href="https://help.github.com" data-ga-click="Footer, go to help, text:help">Help</a></li>
    </ul>

    <a href="https://github.com" aria-label="Homepage" class="footer-octicon" title="GitHub">
      <svg aria-hidden="true" class="octicon octicon-mark-github" height="24" version="1.1" viewBox="0 0 16 16" width="24"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"/></svg>
</a>
    <ul class="list-style-none d-flex flex-wrap ">
        <li class="mr-3"><a href="https://github.com/contact" data-ga-click="Footer, go to contact, text:contact">Contact GitHub</a></li>
      <li class="mr-3"><a href="https://developer.github.com" data-ga-click="Footer, go to api, text:api">API</a></li>
      <li class="mr-3"><a href="https://training.github.com" data-ga-click="Footer, go to training, text:training">Training</a></li>
      <li class="mr-3"><a href="https://shop.github.com" data-ga-click="Footer, go to shop, text:shop">Shop</a></li>
        <li class="mr-3"><a href="https://github.com/blog" data-ga-click="Footer, go to blog, text:blog">Blog</a></li>
        <li><a href="https://github.com/about" data-ga-click="Footer, go to about, text:about">About</a></li>

    </ul>
  </div>
</div>



  <div id="ajax-error-message" class="ajax-error-message flash flash-error">
    <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.865 1.52c-.18-.31-.51-.5-.87-.5s-.69.19-.87.5L.275 13.5c-.18.31-.18.69 0 1 .19.31.52.5.87.5h13.7c.36 0 .69-.19.86-.5.17-.31.18-.69.01-1L8.865 1.52zM8.995 13h-2v-2h2v2zm0-3h-2V6h2v4z"/></svg>
    <button type="button" class="flash-close js-flash-close js-ajax-error-dismiss" aria-label="Dismiss error">
      <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
    </button>
    You can't perform that action at this time.
  </div>


    <script crossorigin="anonymous" src="https://assets-cdn.github.com/assets/compat-91f98c37fc84eac24836eec2567e9912742094369a04c4eba6e3cd1fa18902d9.js"></script>
    <script crossorigin="anonymous" src="https://assets-cdn.github.com/assets/frameworks-b091f3059f3cbf6e49f0bcd7adcb3eb79f1e8efb9a57d4d35f3cec4535f2b80a.js"></script>
    
    <script async="async" crossorigin="anonymous" src="https://assets-cdn.github.com/assets/github-bdbb6560198d10308c03de469bbabc5945f2d8873f6a78521e87ce053b1b585e.js"></script>
    
    
    
    
  <div class="js-stale-session-flash stale-session-flash flash flash-warn flash-banner d-none">
    <svg aria-hidden="true" class="octicon octicon-alert" height="16" version="1.1" viewBox="0 0 16 16" width="16"><path fill-rule="evenodd" d="M8.865 1.52c-.18-.31-.51-.5-.87-.5s-.69.19-.87.5L.275 13.5c-.18.31-.18.69 0 1 .19.31.52.5.87.5h13.7c.36 0 .69-.19.86-.5.17-.31.18-.69.01-1L8.865 1.52zM8.995 13h-2v-2h2v2zm0-3h-2V6h2v4z"/></svg>
    <span class="signed-in-tab-flash">You signed in with another tab or window. <a href="">Reload</a> to refresh your session.</span>
    <span class="signed-out-tab-flash">You signed out in another tab or window. <a href="">Reload</a> to refresh your session.</span>
  </div>
  <div class="facebox" id="facebox" style="display:none;">
  <div class="facebox-popup">
    <div class="facebox-content" role="dialog" aria-labelledby="facebox-header" aria-describedby="facebox-description">
    </div>
    <button type="button" class="facebox-close js-facebox-close" aria-label="Close modal">
      <svg aria-hidden="true" class="octicon octicon-x" height="16" version="1.1" viewBox="0 0 12 16" width="12"><path fill-rule="evenodd" d="M7.48 8l3.75 3.75-1.48 1.48L6 9.48l-3.75 3.75-1.48-1.48L4.52 8 .77 4.25l1.48-1.48L6 6.52l3.75-3.75 1.48 1.48z"/></svg>
    </button>
  </div>
</div>


  </body>
</html>

