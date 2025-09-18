#![allow(dead_code)]

use std::cmp::{self, Ordering};
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::sync::{Arc, LazyLock, Mutex};

use regex::Regex;

#[derive(Debug, Eq, PartialEq, Clone)]
struct Piece {
	// Can't use 'type' as a name variable.
	#[allow(clippy::struct_field_names)]
	piece_type: PieceType,
	color: Color,
	move_counter: usize,
}
// Constants.
impl Piece {
	const WHITE_KING: Piece = Piece {
		piece_type: PieceType::King,
		color: Color::White,
		move_counter: 0,
	};
	const WHITE_QUEEN: Piece = Piece {
		piece_type: PieceType::Queen,
		color: Color::White,
		move_counter: 0,
	};
	const WHITE_ROOK: Piece = Piece {
		piece_type: PieceType::Rook,
		color: Color::White,
		move_counter: 0,
	};
	const WHITE_BISHOP: Piece = Piece {
		piece_type: PieceType::Bishop,
		color: Color::White,
		move_counter: 0,
	};
	const WHITE_KNIGHT: Piece = Piece {
		piece_type: PieceType::Knight,
		color: Color::White,
		move_counter: 0,
	};
	const WHITE_PAWN: Piece = Piece {
		piece_type: PieceType::Pawn,
		color: Color::White,
		move_counter: 0,
	};

	const BLACK_KING: Piece = Piece {
		piece_type: PieceType::King,
		color: Color::Black,
		move_counter: 0,
	};
	const BLACK_QUEEN: Piece = Piece {
		piece_type: PieceType::Queen,
		color: Color::Black,
		move_counter: 0,
	};
	const BLACK_ROOK: Piece = Piece {
		piece_type: PieceType::Rook,
		color: Color::Black,
		move_counter: 0,
	};
	const BLACK_BISHOP: Piece = Piece {
		piece_type: PieceType::Bishop,
		color: Color::Black,
		move_counter: 0,
	};
	const BLACK_KNIGHT: Piece = Piece {
		piece_type: PieceType::Knight,
		color: Color::Black,
		move_counter: 0,
	};
	const BLACK_PAWN: Piece = Piece {
		piece_type: PieceType::Pawn,
		color: Color::Black,
		move_counter: 0,
	};

	const MARK: Piece = Piece {
		piece_type: PieceType::Mark,
		color: Color::Mark,
		move_counter: 0,
	};
}
// Functions.
impl Piece {
	fn get_symbol(&self) -> String {
		let opening_symbol = self.color.get_opening_symbol();
		let piece_symbol = self.piece_type.get_symbol();
		let closing_symbol = self.color.get_closing_symbol();
		format!("{opening_symbol}{piece_symbol}{closing_symbol}")
	}
	fn is_piece_type(&self, piece_type: &PieceType) -> bool {
		self.piece_type == *piece_type
	}
	fn is_color(&self, color: &Color) -> bool {
		self.color == *color
	}
	fn get_destinations(
		&self,
		pos: &Pos,
		pieces: &HashMap<Pos, &Piece>,
		en_passant: Option<&Pos>,
	) -> Vec<Pos> {
		match self.piece_type {
			PieceType::King => king_destinations(pos, &self.color, pieces),
			PieceType::Queen => queen_destinations(pos, &self.color, pieces),
			PieceType::Rook => rook_destinations(pos, &self.color, pieces),
			PieceType::Bishop => bishop_destinations(pos, &self.color, pieces),
			PieceType::Knight => knight_destinations(pos, &self.color, pieces),
			PieceType::Pawn => {
				pawn_destinations(self.move_counter, pos, &self.color, pieces, en_passant)
			}
			PieceType::Mark => Vec::new(),
		}
	}
}
impl fmt::Display for Piece {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.get_symbol())
	}
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct Pos(usize, usize);
impl Pos {
	fn new(file: usize, line: usize) -> Option<Self> {
		if file >= 8 || line >= 8 {
			return None;
		}

		Some(Self(file, line))
	}

	fn eq(&self, other: &Pos) -> bool {
		self.0 == other.0 && self.1 == other.1
	}
}

fn king_destinations(pos: &Pos, color: &Color, pieces: &HashMap<Pos, &Piece>) -> Vec<Pos> {
	let mut result: Vec<Pos> = Vec::new();

	let min_file = pos.0.saturating_sub(1);
	let max_file = cmp::min(7, pos.0 + 1);
	let min_line = pos.1.saturating_sub(1);
	let max_line = cmp::min(7, pos.1 + 1);

	for i in min_file..=max_file {
		for j in min_line..=max_line {
			let square_pos = Pos(i, j);
			if i == pos.0 && j == pos.1 {
				continue;
			}

			if let Some(piece) = pieces.get(&square_pos) {
				if piece.color != *color {
					result.push(square_pos);
				}
			} else {
				result.push(square_pos);
			}
		}
	}

	result
}

fn queen_destinations(pos: &Pos, color: &Color, pieces: &HashMap<Pos, &Piece>) -> Vec<Pos> {
	let mut result: Vec<Pos> = Vec::new();

	result.append(&mut rook_destinations(pos, color, pieces));
	result.append(&mut bishop_destinations(pos, color, pieces));

	result
}

fn rook_destinations(pos: &Pos, color: &Color, pieces: &HashMap<Pos, &Piece>) -> Vec<Pos> {
	let mut result: Vec<Pos> = Vec::new();

	// Horizontal, pos to 0.
	for i in (0..pos.0).rev() {
		let square_pos = Pos(i, pos.1);
		if let Some(piece) = pieces.get(&square_pos) {
			if piece.color != *color {
				result.push(square_pos);
			}

			break;
		}

		result.push(square_pos);
	}

	// Horizontal, pos to 8.
	for i in pos.0 + 1..8 {
		let square_pos = Pos(i, pos.1);
		if let Some(piece) = pieces.get(&square_pos) {
			if piece.color != *color {
				result.push(square_pos);
			}

			break;
		}

		result.push(square_pos);
	}

	for i in (0..pos.1).rev() {
		let square_pos = Pos(pos.0, i);
		if let Some(piece) = pieces.get(&square_pos) {
			if piece.color != *color {
				result.push(square_pos);
			}

			break;
		}

		result.push(square_pos);
	}

	for i in pos.1 + 1..8 {
		let square_pos = Pos(pos.0, i);
		if let Some(piece) = pieces.get(&square_pos) {
			if piece.color != *color {
				result.push(square_pos);
			}

			break;
		}

		result.push(square_pos);
	}

	result
}

fn bishop_destinations(pos: &Pos, color: &Color, pieces: &HashMap<Pos, &Piece>) -> Vec<Pos> {
	let mut result: Vec<Pos> = Vec::new();

	// To top right.
	let mut i = 1;
	while pos.0 + i < 8 && pos.1 + i < 8 {
		let file = pos.0 + i;
		let line = pos.1 + i;

		if let Some(piece) = pieces.get(&Pos(file, line)) {
			if piece.color != *color {
				result.push(Pos(file, line));
			}

			break;
		}

		result.push(Pos(file, line));
		i += 1;
	}

	// To bottom left.
	let mut i = 1;
	while let (Some(file), Some(line)) = (pos.0.checked_sub(i), pos.1.checked_sub(i)) {
		if let Some(piece) = pieces.get(&Pos(file, line)) {
			if piece.color != *color {
				result.push(Pos(file, line));
			}

			break;
		}

		result.push(Pos(file, line));
		i += 1;
	}

	// To bottom right.
	let mut i = 1;
	while let Some(line) = pos.1.checked_sub(i) {
		let file = pos.0 + i;
		if file >= 8 {
			break;
		}

		if let Some(piece) = pieces.get(&Pos(file, line)) {
			if piece.color != *color {
				result.push(Pos(file, line));
			}

			break;
		}

		result.push(Pos(file, line));
		i += 1;
	}

	// To top left.
	let mut i = 1;
	while let Some(file) = pos.0.checked_sub(i) {
		let line = pos.1 + i;
		if line >= 8 {
			break;
		}

		if let Some(piece) = pieces.get(&Pos(file, line)) {
			if piece.color != *color {
				result.push(Pos(file, line));
			}

			break;
		}

		result.push(Pos(file, line));
		i += 1;
	}

	result
}

fn knight_destinations(pos: &Pos, color: &Color, pieces: &HashMap<Pos, &Piece>) -> Vec<Pos> {
	let mut result: Vec<Pos> = Vec::new();

	let file = pos.0;
	let line = pos.1;

	// 2 up, 1 left.
	if line + 2 < 8 && file > 0 {
		let pos = Pos(file - 1, line + 2);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	// 2 up, 1 right.
	if line + 2 < 8 && file + 1 < 8 {
		let pos = Pos(file + 1, line + 2);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	// 2 left, 1 up.
	if file > 1 && line + 1 < 8 {
		let pos = Pos(file - 2, line + 1);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	// 2 left, 1 down.
	if file > 1 && line > 0 {
		let pos = Pos(file - 2, line - 1);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	// 2 bottom, 1 left
	if file > 0 && line > 1 {
		let pos = Pos(file - 1, line - 2);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	// 2 bottom, 1 right.
	if file + 1 < 8 && line > 1 {
		let pos = Pos(file + 1, line - 2);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	// 2 right, 1 bottom.
	if file < 6 && line > 0 {
		let pos = Pos(file + 2, line - 1);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	// 2 right, 1 up.
	if file < 7 && line < 7 {
		let pos = Pos(file + 2, line + 1);

		if let Some(piece) = pieces.get(&pos) {
			if piece.color != *color {
				result.push(pos);
			}
		} else {
			result.push(pos);
		}
	}

	result
}

fn pawn_destinations(
	move_counter: usize,
	pos: &Pos,
	color: &Color,
	pieces: &HashMap<Pos, &Piece>,
	en_passant: Option<&Pos>,
) -> Vec<Pos> {
	match color {
		Color::White => white_pawn_destinations(move_counter, pos, pieces, en_passant),
		Color::Black => black_pawn_destinations(move_counter, pos, pieces, en_passant),
		Color::Mark => panic!("Invalid color."),
	}
}

fn white_pawn_destinations(
	move_counter: usize,
	pos: &Pos,
	pieces: &HashMap<Pos, &Piece>,
	en_passant: Option<&Pos>,
) -> Vec<Pos> {
	let mut result: Vec<Pos> = Vec::new();

	let square_above = Pos(pos.0, pos.1 + 1);
	if pieces.get(&square_above).is_none() {
		result.push(square_above);

		let square_above_above = Pos(pos.0, pos.1 + 2);
		if move_counter == 0 && pieces.get(&square_above_above).is_none() {
			result.push(square_above_above);
		}
	}

	// Pos(file, line)

	if pos.0 > 0 {
		let square_top_left = Pos(pos.0 - 1, pos.1 + 1);
		if let Some(piece) = pieces.get(&square_top_left) {
			if piece.color != Color::White {
				result.push(square_top_left);
			}
		}
		if let Some(en_passant_pos) = en_passant {
			if en_passant_pos.1 - 1 == pos.1
				&& en_passant_pos.0 == pos.0 - 1
			{
				result.push(en_passant_pos.clone());
			}
		}
	}

	if pos.0 < 7 {
		let square_top_right = Pos(pos.0 + 1, pos.1 + 1);
		if let Some(piece) = pieces.get(&square_top_right) {
			if piece.color != Color::White {
				result.push(square_top_right);
			}
		}
		if let Some(en_passant_pos) = en_passant {
			if en_passant_pos.1 - 1 == pos.1
				&& en_passant_pos.0 == pos.0 + 1
			{
				result.push(en_passant_pos.clone());
			}
		}
	}

	result
}

fn black_pawn_destinations(
	move_counter: usize,
	pos: &Pos,
	pieces: &HashMap<Pos, &Piece>,
	en_passant: Option<&Pos>,
) -> Vec<Pos> {
	let mut result: Vec<Pos> = Vec::new();

	let square_above = Pos(pos.0, pos.1 - 1);
	if pieces.get(&square_above).is_none() {
		result.push(square_above);

		let square_above_above = Pos(pos.0, pos.1 - 2);
		if move_counter == 0 && pieces.get(&square_above_above).is_none() {
			result.push(square_above_above);
		}
	}

	if pos.0 > 0 {
		let square_top_left = Pos(pos.0 - 1, pos.1 - 1);
		if let Some(piece) = pieces.get(&square_top_left) {
			if piece.color != Color::Black {
				result.push(square_top_left);
			}
		}
		if let Some(en_passant_pos) = en_passant {
			if en_passant_pos.1 + 1 == pos.1
				&& en_passant_pos.0 == pos.0 - 1
				{
					result.push(en_passant_pos.clone());
				}
		}
	}

	if pos.0 < 7 {
		let square_top_right = Pos(pos.0 + 1, pos.1 - 1);
		if let Some(piece) = pieces.get(&square_top_right) {
			if piece.color != Color::Black {
				result.push(square_top_right);
			}
		}
		if let Some(en_passant_pos) = en_passant {
			if en_passant_pos.1 + 1 == pos.1
				&& en_passant_pos.0 == pos.0 + 1
				{
					result.push(en_passant_pos.clone());
				}
		}
	}

	result
}

fn is_in_board(pos: &Pos) -> bool {
	pos.0 < 8 && pos.1 < 8
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum PieceType {
	King,
	Queen,
	Rook,
	Bishop,
	Knight,
	Pawn,

	Mark,
}
impl PieceType {
	fn from_symbol(symbol: &str) -> Self {
		match symbol {
			"K" => PieceType::King,
			"Q" => PieceType::Queen,
			"R" => PieceType::Rook,
			"B" => PieceType::Bishop,
			"N" => PieceType::Knight,
			_ => PieceType::Pawn,
		}
	}
	fn get_symbol(&self) -> String {
		match self {
			PieceType::King => "K".to_owned(),
			PieceType::Queen => "Q".to_owned(),
			PieceType::Rook => "R".to_owned(),
			PieceType::Bishop => "B".to_owned(),
			PieceType::Knight => "N".to_owned(),
			PieceType::Pawn => "P".to_owned(),
			PieceType::Mark => '*'.to_string(),
		}
	}
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Color {
	White,
	Black,

	Mark,
}
impl Color {
	fn invert(color: &Color) -> Color {
		match color {
			Color::White => Color::Black,
			Color::Black => Color::White,
			Color::Mark => panic!("Can't be inverted."),
		}
	}
	fn get_opening_symbol(&self) -> String {
		match self {
			Color::White => "<".to_owned(),
			Color::Black => "[".to_owned(),
			Color::Mark => " ".to_string(),
		}
	}
	fn get_closing_symbol(&self) -> String {
		match self {
			Color::White => ">".to_owned(),
			Color::Black => "]".to_owned(),
			Color::Mark => ' '.to_string(),
		}
	}
}

#[derive(Clone)]
struct Board {
	grid: Vec<Vec<Option<Piece>>>,
	playing: Color,
	move_list: Vec<MoveType>,
	en_passant: Option<Pos>,
}
impl Board {
	fn new() -> Self {
		let mut grid: Vec<Vec<Option<Piece>>> = Vec::new();
		for _ in 0..8 {
			let mut line: Vec<Option<Piece>> = Vec::new();

			for _ in 0..8 {
				line.push(None);
			}

			grid.push(line);
		}

		{
			grid[0][0] = Some(Piece::WHITE_ROOK);
			grid[0][1] = Some(Piece::WHITE_KNIGHT);
			grid[0][2] = Some(Piece::WHITE_BISHOP);
			grid[0][3] = Some(Piece::WHITE_QUEEN);

			grid[0][4] = Some(Piece::WHITE_KING);
			grid[0][5] = Some(Piece::WHITE_BISHOP);
			grid[0][6] = Some(Piece::WHITE_KNIGHT);
			grid[0][7] = Some(Piece::WHITE_ROOK);

			grid[1][0] = Some(Piece::WHITE_PAWN);
			grid[1][1] = Some(Piece::WHITE_PAWN);
			grid[1][2] = Some(Piece::WHITE_PAWN);
			grid[1][3] = Some(Piece::WHITE_PAWN);

			grid[1][4] = Some(Piece::WHITE_PAWN);
			grid[1][5] = Some(Piece::WHITE_PAWN);
			grid[1][6] = Some(Piece::WHITE_PAWN);
			grid[1][7] = Some(Piece::WHITE_PAWN);
		}

		{
			grid[7][0] = Some(Piece::BLACK_ROOK);
			grid[7][1] = Some(Piece::BLACK_KNIGHT);
			grid[7][2] = Some(Piece::BLACK_BISHOP);
			grid[7][3] = Some(Piece::BLACK_QUEEN);

			grid[7][4] = Some(Piece::BLACK_KING);
			grid[7][5] = Some(Piece::BLACK_BISHOP);
			grid[7][6] = Some(Piece::BLACK_KNIGHT);
			grid[7][7] = Some(Piece::BLACK_ROOK);

			grid[6][0] = Some(Piece::BLACK_PAWN);
			grid[6][1] = Some(Piece::BLACK_PAWN);
			grid[6][2] = Some(Piece::BLACK_PAWN);
			grid[6][3] = Some(Piece::BLACK_PAWN);

			grid[6][4] = Some(Piece::BLACK_PAWN);
			grid[6][5] = Some(Piece::BLACK_PAWN);
			grid[6][6] = Some(Piece::BLACK_PAWN);
			grid[6][7] = Some(Piece::BLACK_PAWN);
		}

		Self {
			grid,
			playing: Color::White,
			move_list: Vec::new(),
			en_passant: None,
		}
	}
	fn new_empty() -> Self {
		let mut grid: Vec<Vec<Option<Piece>>> = Vec::new();
		for _ in 0..8 {
			let mut line: Vec<Option<Piece>> = Vec::new();

			for _ in 0..8 {
				line.push(None);
			}

			grid.push(line);
		}

		Self {
			grid,
			playing: Color::White,
			move_list: Vec::new(),
			en_passant: None,
		}
	}
	fn get_square_color(line: usize, file: usize) -> String {
		if (line + file) % 2 == 1 {
			"   ".to_owned()
		} else {
			":::".to_owned()
		}
	}
	fn get_pieces(
		&self,
		piece_type: Option<&PieceType>,
		color: Option<&Color>,
	) -> HashMap<Pos, &Piece> {
		let mut result: HashMap<Pos, &Piece> = HashMap::new();

		for line in 0..8 {
			for file in 0..8 {
				if let Some(piece) = &self.grid[line][file] {
					result.insert(Pos(file, line), piece);
				}
			}
		}

		if let Some(piece_type) = piece_type {
			result.retain(|_pos, piece| piece.is_piece_type(piece_type));
		}

		if let Some(color) = color {
			result.retain(|_pos, piece| piece.is_color(color));
		}

		result
	}
	fn user_move(&mut self, user_move: &MoveType) -> Result<(), ChessError> {
		let pieces_movement: Vec<Movement> = match user_move {
			MoveType::KingSideCastling => {
				if can_kingside_castling(&self.playing, &self.get_pieces(None, None)) {
					kingside_castling_movements(&self.playing)
				} else {
					return Err(ChessError::CantCastling);
				}
			}
			MoveType::QueenSideCastling => {
				if can_queenside_castling(&self.playing, &self.get_pieces(None, None)) {
					queenside_castling_movements(&self.playing)
				} else {
					return Err(ChessError::CantCastling);
				}
			}
			MoveType::PieceMove(chess_notation) => piece_movements(self, chess_notation)?,
		};

		for mv in pieces_movement {
			let mut piece = self.remove_piece(&mv.origin).unwrap();
			match pawn_promotion(&mut piece, user_move) {
				Ok(_) => {}
				Err(error) => {
					self.put_piece(piece, &mv.origin);
					return Err(error);
				}
			}
			piece.move_counter += 1;
			self.put_piece(piece, &mv.destination);
		}

		self.en_passant = get_pawn_en_passant_case_pos(user_move, &self.playing);
		self.playing = Color::invert(&self.playing);
		self.move_list.push(user_move.clone());

		Ok(())
	}
	fn create_next(&self, user_move: &MoveType) -> Result<Self, ChessError> {
		let mut future = self.clone();
		future.user_move(user_move)?;
		Ok(future)
	}
	fn mark_destinations(&mut self, pos: &Vec<Pos>) {
		let mark_piece = Piece::MARK;
		for pos in pos {
			self.put_piece(mark_piece.clone(), pos);
		}
	}
	fn remove_piece(&mut self, pos: &Pos) -> Option<Piece> {
		let piece = self.grid[pos.1][pos.0].clone();
		self.grid[pos.1][pos.0] = None;
		piece
	}
	fn put_piece(&mut self, piece: Piece, pos: &Pos) {
		self.grid[pos.1][pos.0] = Some(piece);
	}
	fn print(&self) {
		println!(" +------------------------+");

		for line in (0..8).rev() {
			print!("{}|", line + 1);

			for file in 0..8 {
				print!("{}", get_symbol(line, file, self.grid[line][file].as_ref()));
			}

			println!("|");
		}

		println!(" +------------------------+");
		println!("   a  b  c  d  e  f  g  h  ");
	}
}

fn get_pawn_en_passant_case_pos(user_move: &MoveType, playing: &Color) -> Option<Pos> {
	// Check if pawn and move by two from starting pos.
	// Pos = case that the pawn jumped.
	match user_move {
		MoveType::KingSideCastling | MoveType::QueenSideCastling => None,
		MoveType::PieceMove(chess_notation) => {
			let Pos(file, line) = chess_notation.dest;
			match (playing, line) {
				(Color::White, 3) => Some(Pos(file, 2)),
				(Color::Black, 4) => Some(Pos(file, 5)),
				_ => None,
			}
		}
	}
}

fn can_kingside_castling(color: &Color, all_pieces: &HashMap<Pos, &Piece>) -> bool {
	// Check white.
	if *color == Color::White
		&& all_pieces.get(&Pos(4, 0)).is_some()
		&& **all_pieces.get(&Pos(4, 0)).unwrap() == Piece::WHITE_KING
		&& all_pieces.get(&Pos(5, 0)).is_none()
		&& all_pieces.get(&Pos(6, 0)).is_none()
		&& all_pieces.get(&Pos(7, 0)).is_some()
		&& **all_pieces.get(&Pos(7, 0)).unwrap() == Piece::WHITE_ROOK
		&& !is_position_attacked(&Pos(4, 0), &Color::Black, all_pieces)
		&& !is_position_attacked(&Pos(5, 0), &Color::Black, all_pieces)
		&& !is_position_attacked(&Pos(6, 0), &Color::Black, all_pieces)
		&& !is_position_attacked(&Pos(7, 0), &Color::Black, all_pieces)
	{
		true
	}
	// Check black.
	else if *color == Color::Black
		&& all_pieces.get(&Pos(4, 7)).is_some()
		&& **all_pieces.get(&Pos(4, 7)).unwrap() == Piece::BLACK_KING
		&& all_pieces.get(&Pos(5, 7)).is_none()
		&& all_pieces.get(&Pos(6, 7)).is_none()
		&& all_pieces.get(&Pos(7, 7)).is_some()
		&& **all_pieces.get(&Pos(7, 7)).unwrap() == Piece::BLACK_ROOK
		&& !is_position_attacked(&Pos(4, 7), &Color::White, all_pieces)
		&& !is_position_attacked(&Pos(5, 7), &Color::White, all_pieces)
		&& !is_position_attacked(&Pos(6, 7), &Color::White, all_pieces)
		&& !is_position_attacked(&Pos(7, 7), &Color::White, all_pieces)
	{
		true
	} else {
		false
	}
}

fn can_queenside_castling(color: &Color, all_pieces: &HashMap<Pos, &Piece>) -> bool {
	// Check white.
	if *color == Color::White
		&& all_pieces.get(&Pos(4, 0)).is_some()
		&& **all_pieces.get(&Pos(4, 0)).unwrap() == Piece::WHITE_KING
		&& all_pieces.get(&Pos(3, 0)).is_none()
		&& all_pieces.get(&Pos(2, 0)).is_none()
		&& all_pieces.get(&Pos(1, 0)).is_none()
		&& all_pieces.get(&Pos(0, 0)).is_some()
		&& **all_pieces.get(&Pos(0, 0)).unwrap() == Piece::WHITE_ROOK
		&& !is_position_attacked(&Pos(4, 0), &Color::Black, all_pieces)
		&& !is_position_attacked(&Pos(3, 0), &Color::Black, all_pieces)
		&& !is_position_attacked(&Pos(2, 0), &Color::Black, all_pieces)
		&& !is_position_attacked(&Pos(1, 0), &Color::Black, all_pieces)
		&& !is_position_attacked(&Pos(0, 0), &Color::Black, all_pieces)
	{
		true
	}
	// Check black.
	else if *color == Color::Black
		&& all_pieces.get(&Pos(4, 7)).is_some()
		&& **all_pieces.get(&Pos(4, 7)).unwrap() == Piece::BLACK_KING
		&& all_pieces.get(&Pos(3, 7)).is_none()
		&& all_pieces.get(&Pos(2, 7)).is_none()
		&& all_pieces.get(&Pos(1, 7)).is_none()
		&& all_pieces.get(&Pos(0, 7)).is_some()
		&& **all_pieces.get(&Pos(0, 7)).unwrap() == Piece::BLACK_ROOK
		&& !is_position_attacked(&Pos(4, 7), &Color::White, all_pieces)
		&& !is_position_attacked(&Pos(3, 7), &Color::White, all_pieces)
		&& !is_position_attacked(&Pos(2, 7), &Color::White, all_pieces)
		&& !is_position_attacked(&Pos(1, 7), &Color::White, all_pieces)
		&& !is_position_attacked(&Pos(0, 7), &Color::White, all_pieces)
	{
		true
	} else {
		false
	}
}

fn is_position_attacked(pos: &Pos, attack_color: &Color, pieces: &HashMap<Pos, &Piece>) -> bool {
	for (piece_pos, piece) in pieces {
		if piece
			.get_destinations(piece_pos, pieces, None)
			.contains(pos)
			&& piece.color == *attack_color
		{
			return true;
		}
	}

	false
}

struct Movement {
	origin: Pos,
	destination: Pos,
}

impl Movement {
	fn from_tuples(o: (usize, usize), d: (usize, usize)) -> Self {
		Self {
			origin: Pos(o.0, o.1),
			destination: Pos(d.0, d.1),
		}
	}
}

fn kingside_castling_movements(color: &Color) -> Vec<Movement> {
	match color {
		Color::White => {
			vec![
				Movement::from_tuples((7, 0), (5, 0)),
				Movement::from_tuples((4, 0), (6, 0)),
			]
		}
		Color::Black => {
			vec![
				Movement::from_tuples((7, 7), (5, 7)),
				Movement::from_tuples((4, 7), (6, 7)),
			]
		}
		Color::Mark => panic!("Invalid color."),
	}
}

fn queenside_castling_movements(color: &Color) -> Vec<Movement> {
	match color {
		Color::White => {
			vec![
				Movement::from_tuples((0, 0), (3, 0)),
				Movement::from_tuples((4, 0), (2, 0)),
			]
		}
		Color::Black => {
			vec![
				Movement::from_tuples((0, 7), (3, 7)),
				Movement::from_tuples((4, 7), (2, 7)),
			]
		}
		Color::Mark => panic!("Invalid color."),
	}
}

fn piece_movements(
	board: &Board,
	chess_notation: &ChessNotation,
) -> Result<Vec<Movement>, ChessError> {
	let mut movements: Vec<Movement> = Vec::new();

	let pieces = board.get_pieces(None, None);

	let filtered_pieces = board.get_pieces(Some(&chess_notation.piece_type), Some(&board.playing));

	let origin_pos = get_piece_position(
		chess_notation,
		&filtered_pieces,
		&pieces,
		board.en_passant.as_ref(),
	)?;

	movements.push(Movement {
		origin: origin_pos,
		destination: chess_notation.dest.clone(),
	});

	Ok(movements)
}

fn get_piece_position(
	chess_notation: &ChessNotation,
	filtered_pieces: &HashMap<Pos, &Piece>,
	pieces: &HashMap<Pos, &Piece>,
	en_passant: Option<&Pos>,
) -> Result<Pos, ChessError> {
	let mut result: Vec<Pos> = Vec::new();

	for (pos, piece) in filtered_pieces {
		let destinations = piece.get_destinations(pos, pieces, en_passant);

		if destinations_contains_pos(&destinations, &chess_notation.dest) {
			if let Some(file) = chess_notation.origin_file {
				if file == pos.0 {
					result.push(pos.clone());
				}
			} else if let Some(line) = chess_notation.origin_line {
				if line == pos.1 {
					result.push(pos.clone());
				}
			} else {
				result.push(pos.clone());
			}
		}
	}

	if result.is_empty() {
		return Err(ChessError::NoPieceCanReachDestination);
	}

	if result.len() > 1 {
		return Err(ChessError::AmbiguousMovement);
	}

	Ok(result[0].clone())
}

fn pawn_promotion(piece: &mut Piece, user_move: &MoveType) -> Result<bool, ChessError> {
	if piece.piece_type == PieceType::Pawn {
		let MoveType::PieceMove(notation) = user_move else {
			unreachable!()
		};

		if notation.dest.1 == 0 || notation.dest.1 == 7 {
			let Some(promotion_piece) = notation.promotion.clone() else {
				return Err(ChessError::MissingPawnPromotion);
			};

			piece.piece_type = promotion_piece;

			Ok(true)
		} else {
			Ok(false)
		}
	} else {
		Ok(false)
	}
}

fn destinations_contains_pos(destinations: &Vec<Pos>, pos: &Pos) -> bool {
	for p in destinations {
		if p.eq(pos) {
			return true;
		}
	}

	false
}

fn get_symbol(line: usize, file: usize, piece: Option<&Piece>) -> String {
	if let Some(piece) = piece {
		piece.get_symbol()
	} else {
		Board::get_square_color(line, file)
	}
}

const CHESS_NOTATION_STR_REGEX: &str = r"(?<piece>K|Q|R|B|N|P?)(?<origin_file>[a-h]?)(?<origin_line>[1-8]?)x?(?<dest_file>[a-h])(?<dest_line>[1-8])=?(?<promotion>K|Q|R|B|N|P?)";

const KINGSIDE_CASTLING_STR_REGEX: &str = "0-0";
const QUEENSIDE_CASTLING_STR_REGEX: &str = "0-0-0";

static CHESS_NOTATION_REGEX: LazyLock<Arc<Mutex<Regex>>> =
	LazyLock::new(|| Arc::new(Mutex::new(Regex::new(CHESS_NOTATION_STR_REGEX).unwrap())));

#[derive(Debug, Clone)]
enum MoveType {
	KingSideCastling,
	QueenSideCastling,
	PieceMove(ChessNotation),
}

#[derive(Debug, Clone)]
struct ChessNotation {
	piece_type: PieceType,
	dest: Pos,
	origin_file: Option<usize>,
	origin_line: Option<usize>,
	promotion: Option<PieceType>,
}

impl ChessNotation {
	fn new(
		piece_type: PieceType,
		dest_file: usize,
		dest_line: usize,
		origin_file: Option<usize>,
		origin_line: Option<usize>,
		promotion: Option<PieceType>,
	) -> Self {
		Self {
			piece_type,
			dest: Pos(dest_file, dest_line),
			origin_file,
			origin_line,
			promotion,
		}
	}
}

#[derive(Debug, PartialEq)]
enum ChessError {
	InvalidChessNotationError,
	AmbiguousMovement,
	NoPieceCanReachDestination,
	MissingPawnPromotion,
	CantCastling,
	PieceIsPinned,
	KingInCheck,
	CantMoveKingHere,
}

fn parse_chess_notation(chess_notation: &str) -> Result<MoveType, ChessError> {
	if chess_notation.eq(KINGSIDE_CASTLING_STR_REGEX) {
		return Ok(MoveType::KingSideCastling);
	}

	if chess_notation.eq(QUEENSIDE_CASTLING_STR_REGEX) {
		return Ok(MoveType::QueenSideCastling);
	}

	let Some(caps) = CHESS_NOTATION_REGEX
		.lock()
		.unwrap()
		.captures(chess_notation)
	else {
		return Err(ChessError::InvalidChessNotationError);
	};

	let piece_type = PieceType::from_symbol(&caps["piece"]);
	let dest_file = file_to_usize(&caps["dest_file"]).unwrap();
	let dest_line = line_to_usize(&caps["dest_line"]).unwrap();
	let origin_file = file_to_usize(&caps["origin_file"]);
	let origin_line = line_to_usize(&caps["origin_line"]);
	let promotion = pawn_promotion_piece(&caps["promotion"]);

	let result = ChessNotation::new(
		piece_type,
		dest_file,
		dest_line,
		origin_file,
		origin_line,
		promotion,
	);

	Ok(MoveType::PieceMove(result))
}

fn file_to_usize(file: &str) -> Option<usize> {
	file.chars().next().map(|value| value as usize - 97)
}

fn line_to_usize(line: &str) -> Option<usize> {
	line.chars().next().map(|value| value as usize - 49)
}

fn pawn_promotion_piece(line: &str) -> Option<PieceType> {
	line.chars()
		.next()
		.map(|symbol| PieceType::from_symbol(&String::from(symbol)))
}

const PLAYS_1: &str = "Nc3 f5
e4 fxe4
Nxe4 Nf6
Nxf6 gxf6
Qh5#";

const PLAYS_2: &str = "Nf3 a5
e3 Ra6
Bxa6 xa6
0-0 d5
c3 d4
cxd4 Qd7
Kh1 h6
b4 xb4
a3 xa3
Bb2 xb2
Ne5 xa1Q
Nc3 Qd1
d5 a5
Rxd1 c5
Kg1 Nf6
Kf1 e6
Ke2 Be7
Kd3 0-0-0";

const PLAYS_3: &str = "e4 Nc6
d4 b6
Nf3 Ba6
Nc3 e6
Bf4 Qg5
Bc4 f6
Qd2 Nge7
Qe2 g6
Qd2 Bh6";

const QUEENSIDE_CASTLING: &str = "";

// TODO: add king check verification.
// TODO: check if the match is finished, check mate.
// TODO: add future board for previous TODOs.
// TODO: add function that returns all possible movement for a player.
// TODO: print board from black side.

// TODO: refactor the code lessen `Board` struct responsability and pass it to a new struct `Game` that will handle game turn, movement resolution and verify check, checkmate, stalemate and other things. Ultimately the `Board` struct should just contains pieces positions.
fn main() {
	let args: Vec<String> = env::args().collect();

	let mut board = Board::new();
	let mut moves = Vec::<String>::new();

	match args.len().cmp(&2) {
		Ordering::Equal => {
			if let Ok(replay_file) = open_replay(&args[1]) {
				replay(&mut board, &mut moves, &replay_file);
			} else {
				println!("No such file '{}'.", args[1]);
			}
		}
		Ordering::Greater => {
			println!("Too much arguments.");
			return;
		}
		Ordering::Less => {}
	}

	board.print();

	let stdin = std::io::stdin();
	let mut buffer = String::new();

	loop {
		buffer.clear();

		println!("{:?}", board.playing);
		if stdin.read_line(&mut buffer).is_err() {
			println!("Error while reading the input, try again.");
			continue;
		}

		let input: Vec<&str> = buffer.split_whitespace().collect();

		match input[0] {
			"help" => help(),
			"exit" => break,
			"moves" => {
				println!("{moves:#?}");
			}
			"save" => {
				if let Some(filepath) = input.get(1) {
					if save(&moves, filepath).is_err() {
						println!("Error while writing to '{filepath}'.");
					}
				} else {
					println!("Pass a filepath.");
				}
			}
			_ => {
				let Ok(chess_notation) = parse_chess_notation(input[0]) else {
					println!("Invalid chess notation: {}", input[0]);
					continue;
				};

				let res = board.user_move(&chess_notation);
				if res.is_err() {
					println!("{res:?}");
				} else {
					moves.push(input[0].to_string());
				}
				board.print();
			}
		}
	}
}

fn validate_move(board: &Board, user_move: &MoveType) -> Result<(), ChessError> {
	// TODO: check move is valid:
	// if pawn reach last line no missing promotion piece

	match user_move {
		MoveType::KingSideCastling => {
			if can_kingside_castling(&board.playing, &board.get_pieces(None, None)) {
				return Ok(());
			} else {
				return Err(ChessError::CantCastling);
			}
		}
		MoveType::QueenSideCastling => {
			if can_queenside_castling(&board.playing, &board.get_pieces(None, None)) {
				return Ok(());
			} else {
				return Err(ChessError::CantCastling);
			}
		}
		MoveType::PieceMove(chess_notation) => {
			let movements: Vec<Movement> = piece_movements(board, chess_notation)?;
			if chess_notation.piece_type == PieceType::Pawn
				&& board.playing == Color::White
				&& chess_notation.dest.1 == 7
				&& chess_notation.promotion.is_none()
			{
				return Err(ChessError::MissingPawnPromotion);
			}

			if chess_notation.piece_type == PieceType::Pawn
				&& board.playing == Color::Black
				&& chess_notation.dest.1 == 0
				&& chess_notation.promotion.is_none()
			{
				return Err(ChessError::MissingPawnPromotion);
			}

			let is_king_in_check_pre_move = is_king_in_check(board, &board.playing);

			let next_board = board.create_next(user_move)?;

			let is_king_in_check_post_move = is_king_in_check(&next_board, &board.playing);

			if !is_king_in_check_pre_move && is_king_in_check_post_move {
				return Err(ChessError::PieceIsPinned);
			}

			if is_king_in_check_pre_move && is_king_in_check_post_move {
				return Err(ChessError::KingInCheck);
			}

			if chess_notation.piece_type == PieceType::King && is_king_in_check_post_move {
				return Err(ChessError::CantMoveKingHere);
			}

			return Ok(());
		}
	}

	todo!()
}

fn is_king_in_check(board: &Board, king_color: &Color) -> bool {
	let king_pos: Pos = board
		.get_pieces(Some(&PieceType::King), Some(king_color))
		.into_iter()
		.next()
		.expect("Need one king.")
		.0;
	is_position_attacked(
		&king_pos,
		&Color::invert(king_color),
		&board.get_pieces(None, None),
	)
}

fn is_king_checkmate(_board: &Board, _king_to_check_color: &Color) -> bool {
	todo!()
}

fn help() {
	println!("This game uses the official notation to move pieces.");
}

fn open_replay(filepath: &str) -> std::io::Result<String> {
	let file = File::open(filepath)?;
	let mut buf_reader = BufReader::new(file);
	let mut contents = String::new();
	buf_reader.read_to_string(&mut contents)?;
	Ok(contents)
}

fn replay(board: &mut Board, moves: &mut Vec<String>, replay: &str) {
	for line in replay.lines() {
		let mut line_split = line.split_whitespace();
		let white = line_split.next().unwrap();

		let Ok(chess_notation) = parse_chess_notation(white) else {
			println!("Invalid chess notation: {white}.");
			return;
		};

		let res = board.user_move(&chess_notation);
		println!("{white}");
		if res.is_err() {
			println!("{res:?}");
		} else {
			moves.push(white.to_string());
		}
		board.print();

		if let Some(black) = line_split.next() {
			let Ok(chess_notation) = parse_chess_notation(black) else {
				println!("Invalid chess notation: {black}.");
				return;
			};

			let res = board.user_move(&chess_notation);
			println!("{black}");
			if res.is_err() {
				println!("{res:?}");
			} else {
				moves.push(black.to_string());
			}
			board.print();
		}
	}
}

fn save(moves: &[String], filepath: &str) -> std::io::Result<()> {
	let mut file = File::create(filepath)?;
	for i in (0..moves.len()).step_by(2) {
		write!(file, "{} ", moves[i])?;
		if let Some(black) = moves.get(i + 1) {
			writeln!(file, "{black}")?;
		}
	}
	Ok(())
}

#[cfg(test)]
mod test_chess {
	use super::*;

	#[test]
	fn test_is_in_board() {
		let middle = Pos(3, 3);
		let top_left = Pos(0, 7);
		let top_right = Pos(7, 7);
		let bottom_left = Pos(0, 0);
		let bottom_right = Pos(7, 0);

		let outside = Pos(8, 8);

		assert!(is_in_board(&middle));
		assert!(is_in_board(&top_left));
		assert!(is_in_board(&top_right));
		assert!(is_in_board(&bottom_left));
		assert!(is_in_board(&bottom_right));

		assert!(!is_in_board(&outside));
	}

	#[test]
	fn test_pieces_equality() {
		let white_king = Piece::WHITE_KING;

		let white_king_2 = Piece {
			color: Color::White,
			piece_type: PieceType::King,
			move_counter: 0,
		};

		assert!(white_king == white_king_2);
	}
}
