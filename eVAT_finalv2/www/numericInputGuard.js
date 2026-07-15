// numericInputGuard.js
//
// Problem: Shiny's numericInput sends a missing value (NA) to the server the
// instant a user deletes every digit in the box - even for a split second
// while they're retyping a new number. Code downstream that does arithmetic,
// indexing, or comparisons directly on that value then either trips a
// validation reset/notification, or errors out and crashes the session.
//
// Fix: patch the numeric input binding on the client so that, whenever the
// box is empty or not a valid number, we keep reporting the last known valid
// number to the server instead of a missing value. The user can still clear
// the box and type any new value they like - the visible field is untouched -
// but Shiny (and everything downstream) never actually sees a missing value.
$(document).on('shiny:connected', function () {

  var entry = Shiny.inputBindings.bindingNames['shiny.numberInput'];
  if (!entry || !entry.binding || entry.binding.__naGuardApplied) return;

  var binding = entry.binding;
  var originalGetValue = binding.getValue;
  var originalSetValue = binding.setValue;

  binding.getValue = function (el) {
    var val = originalGetValue.call(this, el);

    var isMissing = (val === null || val === undefined ||
      (typeof val === 'number' && isNaN(val)));

    if (isMissing) {
      if (el.dataset.naGuardLastValid !== undefined) {
        return Number(el.dataset.naGuardLastValid);
      }
      // No value has ever been recorded yet (e.g. very first render) -
      // fall back to the value the input was initialised with.
      var fallback = Number(el.getAttribute('value'));
      return isNaN(fallback) ? val : fallback;
    }

    el.dataset.naGuardLastValid = val;
    return val;
  };

  // Keep the cache in sync whenever the server itself updates the value
  // (e.g. via updateNumericInput), so the "last valid" value never goes stale.
  binding.setValue = function (el, value) {
    if (value !== null && value !== undefined && !isNaN(Number(value))) {
      el.dataset.naGuardLastValid = value;
    }
    return originalSetValue.call(this, el, value);
  };

  binding.__naGuardApplied = true;
});
