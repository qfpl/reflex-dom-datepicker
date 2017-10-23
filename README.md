# Reflex Dom Date Picker

This is a date picker widget, built using [reflex](https://github.com/reflex-frp/reflex) and [reflex-dom](https://github.com/reflex-frp/reflex-dom).

#### Simple case
```haskell
datePickerSimple
  :: MonadWidget t m
  => DateInputConfig t
  -> m (DateInput t)
```

#### Provide your own ``MonadWidget`` wrappers
```haskell
datePickerWrappedWith
  :: MonadWidget t m
  => Wrap DatePickerW t m -- ^ Wrap the whole widget
  -> Wrap ControlW t m    -- ^ Wrap the controls - 'TextInput' and both buttons
  -> Wrap MonthBtnW t m   -- ^ Wrap the month buttons
  -> Wrap DayW t m        -- ^ Wrap the individual 'Day' elements
  -> Wrap DayListW t m    -- ^ Wrap the list of 'Day' elements
  -> DateInputConfig t    -- ^ Configuration
  -> m (DateInput t)
```

#### Use only the components you want

The datepicker modules provide three primary pieces of the underlying widget:
- Core : The core update logic of the datepicker, including parsing and moving to the next or previous month.
- Controls : The input controls for a datepicker, two buttons to move forward and backward, as well as the embedded ``TextInput`` specialised for a ``Day`` type.
- DaySelect : Display a list of ``Day`` values and provide an ``Event`` when one of them is clicked.

Related blog posts:

- [Part 1](https://blog.qfpl.io/posts/reflex/widget/growing-a-date-picker-1/)
- [Part 2](https://blog.qfpl.io/posts/reflex/widget/growing-a-date-picker-2/)
