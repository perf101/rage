// Include this at the end of body, so DOM is loaded

// First 3 rows are fixed text (product, titles, comparison) - all the rest are data
const rows = Array.from(document.querySelector('table').querySelectorAll('tr'));
rows.splice(0,3);

// Helper methods for filter code
const normalise = s =>
  // Normalise a cell value like (-7%) to 7, like (3%) to 3
  parseInt(s.replace('(', '').replace('%)', '').replace('-', '').replace('+', ''), 10);

const is_cell_empty = cell => 
  // If empty then the text content starts with a - followed by a space (as opposed to a negative number)
  cell.textContent.trim().startsWith('- ');

const is_last_cell_empty = row =>
  is_cell_empty(row.lastElementChild);

const is_row_green = row =>
  row.lastElementChild.querySelector('div span').style.color == 'green';

const showAllRows = () => rows.map(row => row.style.display = '');

const getSelectValue = (s) => s.options[s.selectedIndex].value;

function hideNonInterestingRows(regressionsOnly=true, threshold=5){
  for(var i=0; i<rows.length; i++){
    const row = rows[i];

    // Filter empty last row
    if(is_last_cell_empty(row)){
      row.style.display = 'none';
      continue;
    }

    // Filter non-regressions if required
    if(regressionsOnly && is_row_green(row)){
      row.style.display = 'none';
      continue
    }

    const last_cell = row.lastElementChild;
    const subs = last_cell.querySelector('div').querySelectorAll('sub');
    // Filter if no % in the last cell
    if(subs.length == 1){
      row.style.display = 'none';
      continue;
    }

    // second sub is the %
    const normalised_percentage = normalise(subs[1].innerHTML);
    if(normalised_percentage < threshold){
      row.style.display = 'none'; 
      continue;
    }
  }
}

// DOM elements for filter UI
const filterEnabled = document.querySelector('input[name=filterEnabled]');
const filterType = document.querySelector('select[name=filterType]');
const minRegression = document.querySelector('input[name=minRegression]');

// DOM event for filtering (e is event, unused)
const filter = (e) => {
  if(!filterEnabled.checked){
    showAllRows();
    return;
  }

  const regressionsOnly = getSelectValue(filterType) === "regressions";  

  const threshold = parseInt(minRegression.value);
  showAllRows(rows);
  hideNonInterestingRows(regressionsOnly, threshold);
};

filterEnabled.onchange = filter;
filterType.onchange = filter;
minRegression.oninput = filter;

// Run the filter on page load too
filter({});
