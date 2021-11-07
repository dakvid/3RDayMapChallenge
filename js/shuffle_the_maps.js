var Shuffle = window.Shuffle;

var MapExplorer = function (element) {
  this.themes = Array.from(document.querySelectorAll('.mapfilter-theme button'));
  this.years = Array.from(document.querySelectorAll('.mapfilter-year button'));
  this.packages = Array.from(document.querySelectorAll('.mapfilter-package button'));
  this.searchTagsInput = document.querySelector('#searchTags');
  this.searchHandleInput = document.querySelector('#searchHandle');
  this.reset = document.querySelector('#mapfilter-reset-button');
  
  this.shuffle = new Shuffle(element, {
    easing: 'cubic-bezier(0.165, 0.840, 0.440, 1.000)',
    sizer: '.my-sizer-element',
    itemSelector: '.map-card',
  });
  
  this.filters = {
    themes: [],
    years: [],
    packages: [],
    tagsTxt: "",
    handleTxt: "",
  };
  
  this._bindEventListeners();
  this.addSorting();
  //this.addSearchFilter();
};

MapExplorer.prototype._bindEventListeners = function () {
  this._onThemeChange = this._handleThemeChange.bind(this);
  this._onYearChange = this._handleYearChange.bind(this);
  this._onPackageChange = this._handlePackageChange.bind(this);
  this._onTagsChange = this._handleTagsChange.bind(this);
  this._onHandleChange = this._handleHandleChange.bind(this);
  this._onResetChange = this._handleResetChange.bind(this);
  
  this.themes.forEach(function (button) {
    button.addEventListener('click', this._onThemeChange);
  }, this);
  
  this.years.forEach(function (button) {
    button.addEventListener('click', this._onYearChange);
  }, this);
  
  this.packages.forEach(function (button) {
    button.addEventListener('click', this._onPackageChange);
  }, this);
  
  this.searchTagsInput.addEventListener('keyup', this._onTagsChange);
  
  this.searchHandleInput.addEventListener('keyup', this._onHandleChange);
  
  this.reset.addEventListener('click', this._onResetChange);
};

MapExplorer.prototype._getCurrentThemeFilters = function () {
  return this.themes.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentYearFilters = function () {
  return this.years.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentPackageFilters = function () {
  return this.packages.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentTagsFilters = function () {
  return this.searchTagsInput.value.toLowerCase();
};

MapExplorer.prototype._getCurrentHandleFilters = function () {
  return this.searchHandleInput.value.toLowerCase();
};


MapExplorer.prototype._handleThemeChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.themes.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.themes = this._getCurrentThemeFilters();
  this.filter();
};

MapExplorer.prototype._handleYearChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.years.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.years = this._getCurrentYearFilters();
  this.filter();
};

MapExplorer.prototype._handlePackageChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  // Do we want to be free-er for packages though?
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.packages.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.packages = this._getCurrentPackageFilters();
  this.filter();
};

MapExplorer.prototype._handleTagsChange = function (evt) {
  this.filters.tagsTxt = this._getCurrentTagsFilters();
  this.filter();
};

MapExplorer.prototype._handleHandleChange = function (evt) {
  this.filters.handleTxt = this._getCurrentHandleFilters();
  this.filter();
};

MapExplorer.prototype._handleResetChange = function (evt) {
  this.themes.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.themes = [];
  
  this.years.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.years = [];
  
  this.packages.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.packages = [];
  
  this.searchTagsInput.value = "";
  this.filters.tagsTxt = "";
  
  this.searchHandleInput.value = "";
  this.filters.handleTxt = "";
  
  this.filter();
};





MapExplorer.prototype.filter = function () {
  if (this.hasActiveFilters()) {
    this.shuffle.filter(this.itemPassesFilters.bind(this));
  } else {
    this.shuffle.filter(Shuffle.ALL_ITEMS);
  }
};

MapExplorer.prototype.hasActiveFilters = function () {
  return Object.keys(this.filters).some(function (key) {
    return this.filters[key].length > 0;
  }, this);
};

MapExplorer.prototype.itemPassesFilters = function (element) {
  var themes = this.filters.themes;         // 0 or 1
  var years = this.filters.years;           // 0 or 1
  var packages = this.filters.packages;     // 0 or 1
  var tagsTxt = this.filters.tagsTxt;   // "" or nonempty
  var handleTxt = this.filters.handleTxt;   // "" or nonempty
  var f_theme = element.getAttribute('data-theme');
  var f_year = element.getAttribute('data-year');
  var package_ = element.getAttribute('data-packages');
  var f_packages = package_.split(',');
  var tags = element.getAttribute('data-tags');
  var handle = element.getAttribute('data-handle');
  
  // If there is an active XXX filter and this map is not in that array
  // Assume always an Array (cf shuffle.js function)
  
  if (themes.length > 0 && !themes.includes(f_theme)) {
    return false;
  }

  if (years.length > 0 && !years.includes(f_year)) {
    return false;
  }

  function testPackage(a_package) {
    return f_packages.includes(a_package);
  }
  if (packages.length > 0 && !packages.some(testPackage)) {
    return false;
  }

  if (tagsTxt.length > 0) {
    return tags.indexOf(tagsTxt) !== -1;
  }

  if (handleTxt.length > 0) {
    return handle.indexOf(handleTxt) !== -1;
  }

  return true;
};






MapExplorer.prototype.addSorting = function () {
  document.querySelector('.sort-options').addEventListener('click', this._handleSortChange.bind(this));
};

MapExplorer.prototype._handleSortChange = function (evt) {
  var value = evt.target.control.value;

  function sortByTheme(element) {
    return element.getAttribute('data-theme');
  }
  
  function sortByDatePosted(element) {
    return element.getAttribute('data-date-posted');
  }
  
  function sortByHandle(element) {
    return element.getAttribute('data-handle');
  }


  var options;
  if (value === 'theme') {
    options = {
      by: sortByTheme,
    };
  } else if (value === 'handle') {
    options = {
      by: sortByHandle,
    };
  } else if (value === 'posted-old-new') {
    options = {
      reverse: false,
      by: sortByDatePosted,
    };
  } else if (value === 'posted-new-old') {
    options = {
      reverse: true,
      by: sortByDatePosted,
    };
  } else {
    options = {};
  }

  this.shuffle.sort(options);
};



document.addEventListener('DOMContentLoaded', function () {
  window.mapex = new MapExplorer(document.querySelector('.my-shuffle-container'));
});
