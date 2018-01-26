#pragma once
#include <cstddef>
#include <utility>
#include <type_traits>
#include <iostream>
#include <string>
#include <limits>

inline constexpr size_t variant_npos = static_cast<size_t>(-1);

template <typename T, typename T0, typename ... Ts>
struct get_ind_by_type {
	static const size_t val = (std::is_same_v<T, T0> ? 0 :
		(get_ind_by_type<T, Ts...>::val == static_cast<size_t>(-1) ? static_cast<size_t>(-1) :
			get_ind_by_type<T, Ts...>::val) + 1);
};

template <typename T, typename T0>
struct get_ind_by_type<T, T0> {
	static const size_t val = std::is_same_v<T, T0> ? 0 : static_cast<size_t>(-1);
};

template <typename T, typename T0, typename ... Ts>
constexpr size_t get_ind_by_type_v = get_ind_by_type<T, T0, Ts...>::val;

template <size_t I, typename T0, typename ... Ts>
struct get_type_by_ind {
	typedef typename get_type_by_ind<I - 1, Ts...>::type type;
};

template <typename T0, typename ... Ts>
struct get_type_by_ind<0, T0, Ts...> {
	typedef T0 type;
};

template <size_t I, typename T0, typename ... Ts>
using get_type_by_ind_t = typename get_type_by_ind<I, T0, Ts...>::type;

template <typename T, typename T0, typename ... Ts>
struct get_cnt {
	static const size_t val = std::is_same_v<T, T0> +get_cnt<T, Ts...>::val;
};

template <typename T, typename T0>
struct get_cnt<T, T0> {
	static const size_t val = std::is_same_v<T, T0>;
};

template <typename T, typename T0, typename ... Ts>
constexpr size_t get_cnt_v = get_cnt<T, T0, Ts...>::val;

template <typename T0, typename ... Ts>
struct variant;

template <size_t I, typename T>
struct variant_alternative;

template <size_t I, typename T>
using variant_alternative_t = typename variant_alternative<I, T>::type;

struct bad_variant_access : std::exception {
	bad_variant_access() noexcept = default;

	const char* what() const noexcept override {
		return "bad_variant_access";
	}
};

template <size_t I, typename... Types>
constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v) {
	if (I == v.index()) {
		return reinterpret_cast<variant_alternative_t<I, variant<Types...>>&>(v.head);
	}
	else {
		throw bad_variant_access();
	}
}

template <size_t I, typename ... Types>
constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v) {
	if (I == v.index()) {
		return reinterpret_cast<variant_alternative_t<I, variant<Types...>>&&>(v.head);
	}
	else {
		throw bad_variant_access();
	}
}

template <size_t I, typename ... Types>
constexpr variant_alternative_t<I, variant<Types...>> const& get(const variant<Types...>& v) {
	if (I == v.index()) {
		return reinterpret_cast<variant_alternative_t<I, variant<Types...>> const&>(v.head);
	}
	else {
		throw bad_variant_access();
	}
}

template <size_t I, typename ... Types>
constexpr variant_alternative_t<I, variant<Types...>> const&& get(const variant<Types...>&& v) {
	if (I == v.index()) {
		return reinterpret_cast<variant_alternative_t<I, variant<Types...>> const&&>(v.head);
	}
	else {
		throw bad_variant_access();
	}
}

template <typename T>
struct variant_size;

template <typename T>
inline constexpr size_t variant_size_v = variant_size<T>::value;

template <typename ... Types>
struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <typename T>
struct variant_size<const T> : variant_size<T> {};

template <typename T>
struct variant_size<volatile T> : variant_size<T> {};

template <typename T>
struct variant_size<const volatile T> : variant_size<T> {};

template <typename T, typename ... Types>
constexpr T& get(variant<Types...>& v) {
	return get<get_ind_by_type_v<T, Types...>>(v);
}

template <typename T, typename ... Types>
constexpr T&& get(variant<Types...>&& v) {
	return get<get_ind_by_type_v<T, Types...>>(std::move(v));
}

template <typename T, typename ... Types>
constexpr const T& get(const variant<Types...>& v) {
	return get<get_ind_by_type_v<T, Types...>>(v);
}

template <typename T, typename ... Types>
constexpr const T&& get(const variant<Types...>&& v) {
	return get<get_ind_by_type_v<T, Types...>>(std::move(v));
}

template <bool trivially_destructible, typename ... Ts>
struct storage {
	void destroy(size_t ind) {} // unreachable

	template <typename STORAGE>
	void copy_construct(size_t ind, STORAGE&& other) {} // unreachable

	template <typename STORAGE>
	void assign_storage(size_t ind, STORAGE&& other) {} // unreachable

	template <typename STORAGE>
	void swap_storages(size_t ind, STORAGE&& other) {} // unreachable

	constexpr storage() = default;

	~storage() = default;
};

template <bool trivially_destructible, typename T0, typename ... Ts>
struct storage<trivially_destructible, T0, Ts...> {
	constexpr storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit storage(std::in_place_index_t<I>, Args&&... args)
		: tail(std::in_place_index_t<I - 1>(), std::forward<Args>(args)...)
	{}

	template <typename ... Args>
	constexpr explicit storage(std::in_place_index_t<0>, Args&&... args)
		: head(std::forward<Args>(args)...)
	{}

	~storage() {};

	template <typename STORAGE>
	void copy_construct(size_t ind, STORAGE&& other) {
		if (ind == 0) {
			new (&head) T0(std::forward<STORAGE>(other).head);
		}
		else {
			tail.copy_construct(ind - 1, std::forward<STORAGE>(other).tail);
		}
	}

	template <typename STORAGE>
	void assign_storage(size_t ind, STORAGE&& other) {
		if (ind == 0) {
			head = std::forward<STORAGE>(other).head;
		}
		else {
			tail.assign_storage(ind - 1, std::forward<STORAGE>(other).tail);
		}
	}

	template <typename STORAGE>
	void swap_storages(size_t ind, STORAGE&& other) {
		if (ind == 0) {
			std::swap(head, other.head);
		}
		else {
			tail.swap_storages(ind - 1, std::forward<STORAGE>(other).tail);
		}
	}

	void destroy(size_t ind) {
		if (ind == 0) {
			head.~T0();
		}
		else {
			tail.destroy(ind - 1);
		}
	}

protected:
	union {
		T0 head;
		storage<false, Ts...> tail;
	};
};

template <typename T0, typename ... Ts>
struct storage<true, T0, Ts...> {
	constexpr storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit storage(std::in_place_index_t<I>, Args&&... args)
		: tail(std::in_place_index_t<I - 1>(), std::forward<Args>(args)...)
	{}

	template <typename ... Args>
	constexpr explicit storage(std::in_place_index_t<0>, Args&&... args)
		: head(std::forward<Args>(args)...)
	{}

	~storage() = default;

	template <typename STORAGE>
	void copy_construct(size_t ind, STORAGE&& other) {
		if (ind == 0) {
			new (&head) T0(std::forward<STORAGE>(other).head);
		}
		else {
			tail.copy_construct(ind - 1, std::forward<STORAGE>(other).tail);
		}
	}

	template <typename STORAGE>
	void assign_storage(size_t ind, STORAGE&& other) {
		if (ind == 0) {
			head = std::forward<STORAGE>(other).head;
		}
		else {
			tail.assign_storage(ind - 1, std::forward<STORAGE>(other).tail);
		}
	}

	template <typename STORAGE>
	void swap_storages(size_t ind, STORAGE&& other) {
		if (ind == 0) {
			std::swap(head, other.head);
		}
		else {
			tail.swap_storages(ind - 1, std::forward<STORAGE>(other).tail);
		}
	}

	void destroy(size_t ind) {
		if (ind == 0) {
			head.~T0();
		}
		else {
			tail.destroy(ind - 1);
		}
	}

protected:
	union {
		T0 head;
		storage<true, Ts...> tail;
	};
};

template <bool trivially_destructible, typename ... Ts>
struct destructible_storage : storage<false, Ts...> {
	using base = storage<false, Ts...>;

	constexpr destructible_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit destructible_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
		, ind(I)
	{}

	constexpr bool valueless_by_exception() const {
		return ind == variant_npos;
	}

	~destructible_storage() {
		this->destroy(ind);
	}

protected:
	size_t ind;
};

template <typename ... Ts>
struct destructible_storage<true, Ts...> : storage<true, Ts...> {
	using base = storage<true, Ts...>;

	constexpr destructible_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit destructible_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
		, ind(I)
	{}

	constexpr bool valueless_by_exception() const {
		return ind == variant_npos;
	}

	~destructible_storage() = default;

protected:
	size_t ind;
};

template <bool movable, bool trivially_destructible, typename ... Ts>
struct movable_storage : destructible_storage<trivially_destructible, Ts...> {
	using base = destructible_storage<trivially_destructible, Ts...>;

	constexpr movable_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit movable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...) {
		this->ind = I;
	}

	movable_storage(movable_storage&&) = delete;
};

template <bool trivially_destructible, typename ... Ts>
struct movable_storage<true, trivially_destructible, Ts...> : destructible_storage<trivially_destructible, Ts...> {
	using base = destructible_storage<trivially_destructible, Ts...>;

	constexpr movable_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit movable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...) {
		this->ind = I;
	}

	movable_storage(movable_storage&& other) noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Ts>...>) {
		if (other.valueless_by_exception()) {
			this->ind = variant_npos;
		}
		else {
			this->copy_construct(other.ind, std::move(other));
			this->ind = other.ind;
		}
	}

	constexpr bool valueless_by_exception() const {
		return this->ind == variant_npos;
	}
};

template <bool copyable, bool movable, bool trivially_destructible, typename ... Ts>
struct copyable_storage : movable_storage<movable, trivially_destructible, Ts...> {
	using base = movable_storage<movable, trivially_destructible, Ts...>;

	constexpr copyable_storage() = default;

	copyable_storage(copyable_storage const&) = delete;

	copyable_storage(copyable_storage&&) = default;

	template <size_t I, typename ... Args>
	constexpr explicit copyable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
	{}
};

template <bool movable, bool trivially_destructible, typename ... Ts>
struct copyable_storage<true, movable, trivially_destructible, Ts...> : movable_storage<movable, trivially_destructible, Ts...> {
	using base = movable_storage<movable, trivially_destructible, Ts...>;

	constexpr copyable_storage() = default;

	copyable_storage(copyable_storage const& other) {
		if (other.valueless_by_exception()) {
			this->ind = variant_npos;
		}
		else {
			this->copy_construct(other.ind, other);
			this->ind = other.ind;
		}
	}

	copyable_storage(copyable_storage&&) = default;

	template <size_t I, typename ... Args>
	constexpr explicit copyable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
	{}
};

template <bool move_assignable, bool copyable, bool movable, bool trivially_destructible, typename ... Ts>
struct move_assignable_storage : copyable_storage<copyable, movable, trivially_destructible, Ts...> {
	using base = copyable_storage<copyable, movable, trivially_destructible, Ts...>;

	constexpr move_assignable_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit move_assignable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>()
			, std::forward<Args>(args)...) {}

	move_assignable_storage(move_assignable_storage&&) = default;
	move_assignable_storage(move_assignable_storage const&) = default;

	move_assignable_storage& operator=(move_assignable_storage&&) = delete;
};

template <bool copyable, bool movable, bool trivially_destructible, typename ... Ts>
struct move_assignable_storage<true, copyable, movable, trivially_destructible, Ts...>
	: copyable_storage<copyable, movable, trivially_destructible, Ts...> {
	using base = copyable_storage<copyable, movable, trivially_destructible, Ts...>;

	constexpr move_assignable_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit move_assignable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>()
			, std::forward<Args>(args)...) {}

	move_assignable_storage(move_assignable_storage&&) = default;
	move_assignable_storage(move_assignable_storage const&) = default;

	move_assignable_storage& operator=(move_assignable_storage&& rhs)
		noexcept(std::conjunction_v<std::is_nothrow_move_constructible<Ts>...> &&
			std::conjunction_v<std::is_nothrow_move_assignable<Ts>...>) {
		if (rhs.valueless_by_exception()) {
			if (!this->valueless_by_exception()) {
				this->destroy(this->ind);
				this->ind = variant_npos;
			}
		}
		else if (this->ind == rhs.ind) {
			this->assign_storage(this->ind, std::move(rhs));
		}
		else {
			try {
				if (!this->valueless_by_exception()) {
					this->destroy(this->ind);
					this->ind = variant_npos;
				}
				this->copy_construct(rhs.ind, std::move(rhs));
				this->ind = rhs.ind;
			}
			catch (...) {}
		}
		return *this;
	}
};

template <bool copy_assignable, bool move_assignable, bool copyable, bool movable, bool trivially_destructible, typename ... Ts>
struct copy_assignable_storage : move_assignable_storage<move_assignable, copyable, movable, trivially_destructible, Ts...> {
	using base = move_assignable_storage<move_assignable, copyable, movable, trivially_destructible, Ts...>;

	constexpr copy_assignable_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit copy_assignable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
	{}

	copy_assignable_storage(copy_assignable_storage&&) = default;
	copy_assignable_storage(copy_assignable_storage const&) = default;
	copy_assignable_storage& operator=(copy_assignable_storage&&) = default;

	copy_assignable_storage& operator=(copy_assignable_storage const&) = delete;
};

template <bool move_assignable, bool copyable, bool movable, bool trivially_destructible, typename ... Ts>
struct copy_assignable_storage<true, move_assignable, copyable, movable, trivially_destructible, Ts...>
	: move_assignable_storage<move_assignable, copyable, movable, trivially_destructible, Ts...> {
	using base = move_assignable_storage<move_assignable, copyable, movable, trivially_destructible, Ts...>;

	constexpr copy_assignable_storage() = default;

	template <size_t I, typename ... Args>
	constexpr explicit copy_assignable_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
	{}

	copy_assignable_storage(copy_assignable_storage&&) = default;
	copy_assignable_storage(copy_assignable_storage const&) = default;
	copy_assignable_storage& operator=(copy_assignable_storage&&) = default;

	copy_assignable_storage& operator=(copy_assignable_storage const& rhs) {
		if (rhs.valueless_by_exception()) {
			if (!this->valueless_by_exception()) {
				this->destroy(this->ind);
				this->ind = variant_npos;
			}
		}
		else if (this->ind == rhs.ind) {
			this->assign_storage(this->ind, rhs);
		}
		else {
			if (!this->valueless_by_exception()) {
				this->destroy(this->ind);
				this->ind = variant_npos;
			}
			try {
				this->copy_construct(rhs.ind, rhs);
				this->ind = rhs.ind;
			}
			catch (...) {
				this->operator=(copy_assignable_storage(rhs));
			}
		}
		return *this;
	}
};

template <bool default_constructible, bool copy_assignable, bool move_assignable, bool copyable, bool movable, bool trivially_destructible, typename T0, typename ... Ts>
struct default_constructible_storage : copy_assignable_storage<copy_assignable, move_assignable, copyable, movable, trivially_destructible, T0, Ts...> {
	default_constructible_storage() = delete;
	using base = copy_assignable_storage<copy_assignable, move_assignable, copyable, movable, trivially_destructible, T0, Ts...>;

	template <size_t I, typename ... Args>
	constexpr explicit default_constructible_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
	{}

	default_constructible_storage(default_constructible_storage&&) = default;
	default_constructible_storage(default_constructible_storage const&) = default;
	default_constructible_storage& operator=(default_constructible_storage&&) = default;
	default_constructible_storage& operator=(default_constructible_storage const& rhs) = default;
};

template <bool move_assignable, bool copy_assignable, bool copyable, bool movable, bool trivially_destructible, typename T0, typename ... Ts>
struct default_constructible_storage<true, copy_assignable, move_assignable, copyable, movable, trivially_destructible, T0, Ts...>
	: copy_assignable_storage<copy_assignable, move_assignable, copyable, movable, trivially_destructible, T0, Ts...> {
	using base = copy_assignable_storage<copy_assignable, move_assignable, copyable, movable, trivially_destructible, T0, Ts...>;

	constexpr default_constructible_storage() noexcept(std::is_nothrow_default_constructible_v<T0>)
		: base(std::in_place_index_t<0>())
	{}

	template <size_t I, typename ... Args>
	constexpr explicit default_constructible_storage(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
	{}

	default_constructible_storage(default_constructible_storage&&) = default;
	default_constructible_storage(default_constructible_storage const&) = default;
	default_constructible_storage& operator=(default_constructible_storage&&) = default;
	default_constructible_storage& operator=(default_constructible_storage const& rhs) = default;
};

template <typename T0, typename ... Ts>
using default_constructible_storage_t = default_constructible_storage<
	std::is_default_constructible<T0>::value,
	std::conjunction_v<std::is_copy_assignable<T0>, std::is_copy_assignable<Ts>...,
	std::is_copy_constructible<T0>, std::is_copy_constructible<Ts>...>,
	std::conjunction_v<std::is_move_assignable<T0>, std::is_move_assignable<Ts>...,
	std::is_move_constructible<T0>, std::is_move_constructible<Ts>...>,
	std::conjunction_v<std::is_copy_constructible<T0>, std::is_copy_constructible<Ts>...>,
	std::conjunction_v<std::is_move_constructible<T0>, std::is_move_constructible<Ts>...>,
	std::conjunction_v<std::is_trivially_destructible<T0>, std::is_trivially_destructible<Ts>...>,
	T0, Ts...>;

template <typename T0, typename ... Ts>
struct variant : default_constructible_storage_t<T0, Ts...> {
	using base = default_constructible_storage_t<T0, Ts...>;

	constexpr variant() = default;

	template <typename T, typename ... Types>
	struct type_chooser : type_chooser<Types...> {
		static T F(T);
		using type_chooser<Types...>::F;
	};

	template <typename T>
	struct type_chooser<T> {
		static T F(T);
	};

	template <typename T, typename = std::enable_if_t<!std::is_same_v<std::decay_t<T>, variant>>>
	constexpr variant(T&& t) noexcept(std::is_nothrow_constructible_v<decltype(type_chooser<T0, Ts...>::F(std::forward<T>(t))), T>)
		: base(std::in_place_index_t<get_ind_by_type_v<decltype(type_chooser<T0, Ts...>::F(std::forward<T>(t))), T0, Ts...>>(), std::forward<T>(t))
	{}

	template <typename T, typename ... Args, typename = std::enable_if_t<get_cnt_v<T, T0, Ts...> == 1 && std::is_constructible_v<T, Args...>>>
	constexpr explicit variant(std::in_place_type_t<T>, Args&&... args)
		: base(std::in_place_index_t<get_ind_by_type_v<T, T0, Ts...>>(), std::forward<Args>(args)...)
	{}

	template <size_t I, typename ... Args,
		typename = std::enable_if_t<I <= sizeof...(Ts) && std::is_constructible_v<get_type_by_ind_t<I, T0, Ts...>, Args...>>>
		constexpr explicit variant(std::in_place_index_t<I>, Args&&... args)
		: base(std::in_place_index_t<I>(), std::forward<Args>(args)...)
	{}

	template <typename T, typename... Args, typename = std::enable_if_t<std::is_constructible_v<T, Args...> && get_cnt_v<T, T0, Ts...> == 1>>
	T& emplace(Args&&... args) {
		return emplace<get_ind_by_type_v<T, T0, Ts...>>(std::forward<Args>(args)...);
	}

	template <size_t I, typename... Args, typename = std::enable_if_t<std::is_constructible_v<get_type_by_ind_t<I, T0, Ts...>, Args...>>>
	variant_alternative_t<I, variant>& emplace(Args&&... args) {
		if (!this->valueless_by_exception()) {
			this->destroy(this->ind);
			this->ind = variant_npos;
		}
		new (this) variant(std::in_place_index_t<I>(), std::forward<Args>(args)...);
		this->ind = I;
		return get<I>(*this);
	}

	void swap(variant& other) noexcept(std::conjunction_v<std::is_nothrow_move_constructible<T0>, std::is_nothrow_move_constructible<Ts>...> &&
		std::conjunction_v<std::is_nothrow_swappable<T0>, std::is_nothrow_swappable<Ts>...>) {
		if (this->valueless_by_exception()) {
			if (!other.valueless_by_exception()) {
				this->copy_construct(other.ind, other);
				this->ind = other.ind;
				other.destroy(other.ind);
				other.ind = variant_npos;
			}
		}
		else if (other.valueless_by_exception()) {
			other.copy_construct(this->ind, *this);
			other.ind = this->ind;
			this->destroy(this->ind);
			this->ind = variant_npos;
		}
		else if (this->ind == other.ind) {
			this->swap_storages(other.ind, other);
		}
		else {
			variant tmp(std::move(*this));
			this->destroy(this->ind);
			this->ind = variant_npos;
			this->copy_construct(other.ind, std::move(other));
			this->ind = other.ind;
			other.destroy(other.ind);
			other.ind = variant_npos;
			other.copy_construct(tmp.index(), std::move(tmp));
			other.ind = tmp.ind;
		}
	}

	constexpr size_t index() const noexcept {
		return this->ind;
	}

	template <size_t I, typename... Types>
	friend constexpr variant_alternative_t<I, variant<Types...>>& get(variant<Types...>& v);

	template <size_t I, typename ... Types>
	friend constexpr variant_alternative_t<I, variant<Types...>>&& get(variant<Types...>&& v);

	template <size_t I, typename ... Types>
	friend constexpr variant_alternative_t<I, variant<Types...>> const& get(const variant<Types...>& v);

	template <size_t I, typename ... Types>
	friend constexpr variant_alternative_t<I, variant<Types...>> const&& get(const variant<Types...>&& v);

	template <size_t I, class... Types>
	friend constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept;

	template <size_t I, typename ... Types>
	friend constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>> get_if(const variant<Types...>* pv) noexcept;

	template <typename T, typename ... Types>
	friend constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept;

	template <typename T, typename ... Types>
	friend constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept;
};




template <typename ... Types, typename = std::enable_if_t<std::conjunction_v<std::is_move_constructible<Types>..., std::is_swappable<Types>...>>>
void swap(variant<Types...>& lhs, variant<Types...>& rhs) noexcept(noexcept(lhs.swap(rhs))) {
	lhs.swap(rhs);
}

template <size_t I, typename... Types>
struct variant_alternative<I, variant<Types...>> {
	typedef typename get_type_by_ind<I, Types...>::type type;
};

template <size_t I, typename T>
struct variant_alternative<I, const T> {
	typedef std::add_const_t<variant_alternative_t<I, T>> type;
};

template <size_t I, typename T>
struct variant_alternative<I, volatile T> {
	typedef std::add_volatile_t<variant_alternative_t<I, T>> type;
};

template <size_t I, typename T>
struct variant_alternative<I, const volatile T> {
	typedef std::add_cv_t<variant_alternative_t<I, T>> type;
};

template <size_t I, class... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...>* pv) noexcept {
	if (pv != nullptr && pv->index() == I) {
		return reinterpret_cast<std::add_pointer_t<variant_alternative_t<I, variant<Types...>>>>(&pv->head);
	}
	else {
		return nullptr;
	}
}

template <size_t I, typename ... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>> get_if(const variant<Types...>* pv) noexcept {
	if (pv != nullptr && pv->index() == I) {
		return reinterpret_cast<std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>>(&pv->head);
	}
	else {
		return nullptr;
	}
}

template <typename T, typename ... Types>
constexpr std::add_pointer_t<T> get_if(variant<Types...>* pv) noexcept {
	constexpr size_t ind = get_ind_by_type_v<T, Types...>;
	if (pv != nullptr && pv->index() == ind) {
		return reinterpret_cast<std::add_pointer_t<T>>(&pv->head);
	}
	else {
		return nullptr;
	}
}

template <typename T, typename ... Types>
constexpr std::add_pointer_t<const T> get_if(const variant<Types...>* pv) noexcept {
	constexpr size_t ind = get_ind_by_type_v<T, Types...>;
	if (pv != nullptr && pv->index() == ind) {
		return reinterpret_cast<std::add_pointer_t<const T>>(&pv->head);
	}
	else {
		return nullptr;
	}
}

template <typename T, typename ... Ts, typename = std::enable_if_t<get_cnt_v<T, Ts...> == 1>>
constexpr bool holds_alternative(const variant<Ts...>& v) noexcept {
	if (v.valueless_by_exception()) {
		return false;
	}
	else {
		return v.index() == get_ind_by_type_v<T, Ts...>;
	}
}

struct monostate { };
constexpr bool operator<(monostate, monostate) noexcept { return false; }
constexpr bool operator>(monostate, monostate) noexcept { return false; }
constexpr bool operator<=(monostate, monostate) noexcept { return true; }
constexpr bool operator>=(monostate, monostate) noexcept { return true; }
constexpr bool operator==(monostate, monostate) noexcept { return true; }
constexpr bool operator!=(monostate, monostate) noexcept { return false; }

template <typename T, size_t cur_dim, size_t ... rest_dims>
struct arr {
	template <typename ... size_ts>
	T get(size_t cur_ind, size_ts... rest_inds) const {
		return data[cur_ind].get(rest_inds...);
	}
	arr<T, rest_dims...> data[cur_dim];
};

template <typename T, size_t cur_dim>
struct arr<T, cur_dim> {
	T get(size_t cur_ind) const {
		return data[cur_ind];
	}
	T data[cur_dim];
};

template <typename ... Variants>
struct sequence;

template <size_t ... seq>
struct sequence_inds;

template <typename ret_type, typename Visitor, typename Variants, typename Pos>
struct func;

template <typename ret_type, typename Visitor, typename ... Variants, size_t ... pos>
struct func<ret_type, Visitor, sequence<Variants...>, sequence_inds<pos...>> {
	static ret_type call(Visitor&& visitor, Variants&&... variants) {
		return std::forward<Visitor>(visitor)(get<pos>(std::forward<Variants>(variants))...);
	}
};

template <typename ret_type, typename Visitor, typename ... Variants>
struct fptr_table {
	using arr_type = arr<ret_type(*)(Visitor, Variants...), variant_size_v<std::remove_reference_t<Variants>>...>;

	template <typename Arr_type, size_t ... pos>
	static constexpr Arr_type& make_table(Arr_type& arr, std::enable_if_t<sizeof...(pos) == sizeof...(Variants)>* = nullptr) {
		return arr = func<ret_type, Visitor, sequence<Variants...>, sequence_inds<pos...>>::call;
	}

	template <typename row_type, size_t ... pos, typename Arr_type>
	static constexpr void call(Arr_type& arr, std::index_sequence<> is) {}

	template <typename Arr_type, size_t ... pos>
	static constexpr Arr_type& make_table(Arr_type& arr, std::enable_if_t<sizeof...(pos) != sizeof...(Variants)>* = nullptr) {
		using row_type = decltype(arr.data[0]);
		using tt = typename get_type_by_ind<sizeof...(pos), std::remove_reference_t<Variants>...>::type;

		call<row_type, pos...>(arr, std::make_index_sequence<variant_size_v<tt>>());
		return arr;
	}

	template <typename row_type, size_t ... pos, size_t ind0, size_t ... inds, typename Arr_type>
	static constexpr void call(Arr_type& arr, std::index_sequence<ind0, inds...> is) {
		make_table<row_type, pos..., ind0>(arr.data[ind0]);
		call<row_type, pos...>(arr, std::index_sequence<inds...>());
	}

	static constexpr arr_type make_true_table() {
		arr_type arr = arr_type();
		make_table(arr);
		return arr;
	}

	static constexpr arr_type table = make_true_table();
};

template <typename Visitor, typename ... Variants>
constexpr decltype(auto) visit(Visitor&& vis, Variants&&... vars) {
	using ret_type = decltype(std::forward<Visitor>(vis)(get<0>(std::forward<Variants>(vars))...));
	constexpr auto table = fptr_table<ret_type, Visitor&&, Variants&&...>::table;
	return table.get(vars.index()...)(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <typename ... Types>
constexpr bool operator==(const variant<Types...>& v, const variant<Types...>& w) {
	if (v.index() != w.index()) {
		return false;
	}
	if (v.valueless_by_exception()) {
		return true;
	}
	return visit([](auto& a, auto& b) constexpr -> bool {
		if constexpr(std::is_same_v<decltype(a), decltype(b)>) {
			return a == b;
		}
		else {
			return false;
		}
	}, v, w);
}

template <typename ... Types>
constexpr bool operator!=(const variant<Types...>& v, const variant<Types...>& w) {
	if (v.index() != w.index()) {
		return true;
	}
	if (v.valueless_by_exception()) {
		return false;
	}
	return visit([](auto& a, auto& b) constexpr -> bool {
		if constexpr(std::is_same_v<decltype(a), decltype(b)>) {
			return a != b;
		}
		else {
			return false;
		}
	}, v, w);
}

template <typename ... Types>
constexpr bool operator<(const variant<Types...>& v, const variant<Types...>& w) {
	if (w.valueless_by_exception()) {
		return false;
	}
	if (v.valueless_by_exception()) {
		return true;
	}
	if (v.index() < w.index()) {
		return true;
	}
	if (v.index() > w.index()) {
		return false;
	}
	return visit([](auto& a, auto& b) constexpr -> bool {
		if constexpr(std::is_same_v<decltype(a), decltype(b)>) {
			return a < b;
		}
		else {
			return false;
		}
	}, v, w);
}

template <typename ... Types>
constexpr bool operator>(const variant<Types...>& v, const variant<Types...>& w) {
	if (v.valueless_by_exception()) {
		return false;
	}
	if (w.valueless_by_exception()) {
		return true;
	}
	if (v.index() > w.index()) {
		return true;
	}
	if (v.index() < w.index()) {
		return false;
	}
	return visit([](auto& a, auto& b) constexpr -> bool {
		if constexpr(std::is_same_v<decltype(a), decltype(b)>) {
			return a > b;
		}
		else {
			return false;
		}
	}, v, w);
}

template <typename ... Types>
constexpr bool operator<=(const variant<Types...>& v, const variant<Types...>& w) {
	if (v.valueless_by_exception()) {
		return true;
	}
	if (w.valueless_by_exception()) {
		return false;
	}
	if (v.index() < w.index()) {
		return true;
	}
	if (v.index() > w.index()) {
		return false;
	}
	return visit([](auto& a, auto& b) constexpr -> bool {
		if constexpr(std::is_same_v<decltype(a), decltype(b)>) {
			return a <= b;
		}
		else {
			return false;
		}
	}, v, w);
}

template <typename ... Types>
constexpr bool operator>=(const variant<Types...>& v, const variant<Types...>& w) {
	if (w.valueless_by_exception()) {
		return true;
	}
	if (v.valueless_by_exception()) {
		return false;
	}
	if (v.index() > w.index()) {
		return true;
	}
	if (v.index() < w.index()) {
		return false;
	}
	return visit([](auto& a, auto& b) constexpr -> bool {
		if constexpr(std::is_same_v<decltype(a), decltype(b)>) {
			return a >= b;
		}
		else {
			return false;
		}
	}, v, w);
}
