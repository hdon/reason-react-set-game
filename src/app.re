[%bs.raw {|require('./app.css')|}];

type color =
  | Red
  | Green
  | Purple;

/* TODO why do i always fail at using fun */
/* http://paletton.com/#uid=32t0u0kllllaFw0g0qFqFg0w0aF */
let colorToRgb = (c: color) =>
  switch (c) {
  | Red => "#d47b6a"
  | Green => "#9ac361"
  | Purple => "#7d498c"
  };

/** returns a name for a given color */
let colorToName = (c: color) =>
  switch (c) {
  | Red => "Red"
  | Green => "Green"
  | Purple => "Purple"
  };

type shade =
  | Empty
  | Shaded
  | Solid;

type shape =
  | Diamond
  | Tilde
  | Pill;

type card = {
  ordinal: int,
  color,
  shade,
  shape,
};

type selectable('a) =
  | Selected('a)
  | Unselected('a);

let isSelected =
  fun
  | Selected(_) => true
  | Unselected(_) => false;

exception DomainError(string);

let colorFromInt = (n: int) =>
  switch (n) {
  | 0 => Red
  | 1 => Green
  | 2 => Purple
  | _ => raise(DomainError("no such color"))
  };

let shadeFromInt = (n: int) =>
  switch (n) {
  | 0 => Empty
  | 1 => Shaded
  | 2 => Solid
  | _ => raise(DomainError("no such shade"))
  };

let shapeFromInt = (n: int) =>
  switch (n) {
  | 0 => Diamond
  | 1 => Tilde
  | 2 => Pill
  | _ => raise(DomainError("no such shape"))
  };

let cardFromInt = (n: int) => {
  ordinal: n mod 3 + 1,
  color: colorFromInt(n / 3 mod 3),
  shape: shapeFromInt(n / 3 / 3 mod 3),
  shade: shadeFromInt(n / 3 / 3 / 3 mod 3),
};

let intOfColor =
  fun
  | Red => 0
  | Green => 1
  | Purple => 2;

let intOfShade =
  fun
  | Empty => 0
  | Shaded => 1
  | Solid => 2;

let intOfShape =
  fun
  | Diamond => 0
  | Tilde => 1
  | Pill => 2;

let intOfCard = c =>
  c.ordinal
  - 1
  + intOfColor(c.color)
  * 3
  + intOfShape(c.shape)
  * 3
  * 3
  + intOfShade(c.shade)
  * 3
  * 3
  * 3;

let handleClick = (_event, _self) => {
  Js.log("HI");
  ();
};

let listToElement = (list: list(ReasonReact.reactElement)) =>
  ReasonReact.arrayToElement(Array.of_list(list));

let rec mapRange = (fn, a, z) =>
  a < z ? [fn(a), ...mapRange(fn, a + 1, z)] : [];

let buildShadedPattern = (c: color) =>
  <pattern
    id=("shadedPattern" ++ colorToName(c))
    width="4"
    height="8"
    patternUnits="userSpaceOnUse"
    patternTransform="rotate(-20)">
    <line stroke=(colorToRgb(c)) y2="8" />
  </pattern>;

let svgPatterns =
  listToElement(mapRange(n => buildShadedPattern(colorFromInt(n)), 0, 3));

let strokeStyle = (c: color, _s: shade) => colorToRgb(c);

let fillStyle = (c: color, s: shade) =>
  switch (s, c) {
  | (Empty, _) => "none"
  | (Shaded, _) => "url(#shadedPattern" ++ colorToName(c) ++ ")"
  | (Solid, _) => colorToRgb(c)
  };

let renderDiamond = (c: color, s: shade) =>
  <g stroke=(strokeStyle(c, s)) strokeWidth="2px" fill=(fillStyle(c, s))>
    <path d="M 0 25 L 50 0 L 100 25 L 50 50 Z" />
  </g>;

let renderPill = (c: color, s: shade) =>
  <g stroke=(strokeStyle(c, s)) strokeWidth="2px" fill=(fillStyle(c, s))>
    <path
      d={|
        M 25 0
        H 75
        A 25 25 0 0 1 75 50
        H 25
        A 25 25 0 0 1 25 0
        Z
      |}
    />
  </g>;

let renderTilde = (c: color, s: shade) =>
  <g stroke=(strokeStyle(c, s)) strokeWidth="2px" fill=(fillStyle(c, s))>
    <path
      d={|
        M  10  10
        C  50  30  50 -10  90  10
        C  90  40  90  40  90  40
        C  50  20  50  60  10  40
        C  10  10  10  10  10  10
        Z
      |}
    />
  </g>;

let renderShape = (shap: shape, c: color, s: shade) =>
  switch (shap) {
  | Diamond => renderDiamond(c, s)
  | Tilde => renderTilde(c, s)
  | Pill => renderPill(c, s)
  };

let glyphComp = (shap: shape, c: color, s: shade, n: int) =>
  <div className="glyph" key=(string_of_int(n))>
    <svg viewBox="-2 -2 104 54" version="1.1">
      <defs> svgPatterns </defs>
      (renderShape(shap, c, s))
    </svg>
  </div>;

let cardComp = (n: int, sc: selectable(card), onClick) => {
  let selected =
    switch (sc) {
    | Selected(_) => true
    | Unselected(_) => false
    };
  let className = selected ? "card selected" : "card";
  let c =
    switch (sc) {
    | Selected(c) => c
    | Unselected(c) => c
    };
  <div className onClick key=(string_of_int(n))>
    (
      listToElement(
        mapRange(n => glyphComp(c.shape, c.color, c.shade, n), 0, c.ordinal),
      )
    )
  </div>;
};

type stateType = {
  deck: list(card),
  discarded: list(card),
  table: list(selectable(card)),
  numSelected: int,
};

/* TODO optimize! */
let drawCard = state => {
  ...state,
  table: [Unselected(List.hd(state.deck)), ...state.table],
  deck: List.tl(state.deck),
};

let xOfSelectable = (x: selectable('a)) =>
  switch (x) {
  | Selected(x) => x
  | Unselected(x) => x
  };

let getSelectedCards = (cards: list(selectable(card))) =>
  cards
  |> List.filter(
       fun
       | Selected(_) => true
       | Unselected(_) => false,
     );

let cardsOfSelectable = (cards: list(selectable(card))) =>
  cards |> List.map(xOfSelectable);

let deselect = (x: selectable('a)) =>
  switch (x) {
  | Selected(x) => Unselected(x)
  | Unselected(x) => Unselected(x)
  };

let validPropSet = (a, b, c) =>
  a == b && b == c || a != b && b != c && a != c;

let validSet = ((a, b, c)) =>
  validPropSet(a.ordinal, b.ordinal, c.ordinal)
  && validPropSet(a.color, b.color, c.color)
  && validPropSet(a.shape, b.shape, c.shape)
  && validPropSet(a.shade, b.shade, c.shade);

/* TODO what to do about the warning here? */
let delistTriple = x => {
  let [a, ...x] = x;
  let [b, ...x] = x;
  let [c, ...x] = x;
  (a, b, c);
};

let selectCard = (state, n) => {
  let selecting = ! (n |> List.nth(state.table) |> isSelected);
  let numSelected = state.numSelected + (selecting ? 1 : (-1));
  let table =
    List.mapi(
      (m, sc) =>
        n == m ?
          switch (sc) {
          | Selected(c) => Unselected(c)
          | Unselected(c) => Selected(c)
          } :
          sc,
      state.table,
    );
  if (numSelected == 3) {
    let selectedCards =
      table |> List.filter(isSelected) |> List.map(xOfSelectable);
    if (selectedCards |> delistTriple |> validSet) {
      let table = table |> List.filter(x => ! isSelected(x));
      let state = {...state, table, numSelected: 0};
      let state = List.length(state.table) < 9 ? drawCard(state) : state;
      let state = List.length(state.table) < 9 ? drawCard(state) : state;
      let state = List.length(state.table) < 9 ? drawCard(state) : state;
      state;
    } else {
      {...state, table: state.table |> List.map(deselect), numSelected: 0};
    };
  } else {
    {...state, table, numSelected};
  };
};

type action =
  | DrawCard
  | SelectCard(int);

let make = _children => {
  ...ReasonReact.reducerComponent("SetGame"),
  initialState: () => (
    {
      let state: stateType = {
        deck: mapRange(cardFromInt, 0, 3 * 3 * 3 * 3) |> Belt.List.shuffle,
        discarded: [],
        table: [],
        numSelected: 0,
      };
      /*Belt.Array.shuffleInPlace(state.deck);*/
      drawCard(
        drawCard(
          drawCard(
            drawCard(
              drawCard(drawCard(drawCard(drawCard(drawCard(state))))),
            ),
          ),
        ),
      );
    }: stateType
  ),
  reducer: (action, state: stateType) =>
    switch (action) {
    | DrawCard => ReasonReact.Update(drawCard(state))
    | SelectCard(n) => ReasonReact.Update(selectCard(state, n))
    },
  render: self =>
    <div className="cards">
      (
        ReasonReact.arrayToElement(
          List.mapi(
            (n, aCard) =>
              cardComp(aCard |> xOfSelectable |> intOfCard, aCard, _ev =>
                self.send(SelectCard(n))
              ),
            self.state.table,
          )
          |> List.rev
          |> Array.of_list,
        )
      )
      <div className="card draw" onClick=(_ev => self.send(DrawCard))>
        <div className="glyph">
          <svg viewBox="-50 -50 100 100" version="1.1">
            <circle fill="green" cx="0" cy="0" r="50" />
            <g
              fill="green"
              stroke="white"
              strokeWidth="18px"
              strokeLinecap="round">
              <path
                d={|
                M -22 0
                L  22 0
                M 0 -22
                L 0  22

              |}
              />
            </g>
          </svg>
        </div>
      </div>
    </div>,
};
