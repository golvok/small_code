
#include "id_type.h++"

#include <iostream>

namespace algo {
	namespace Train {
		struct TrainIdTagType {
			const static uint DEFAULT_VALUE = -1;
		};
		using TrainId = ID<uint,TrainIdTagType>;
	}
}

struct StationIdTag {
	static const uint DEFAULT_VALUE = -1;
};
using StationId = ID<uint, StationIdTag>;

struct LocationIdTag { static const uint DEFAULT_VALUE = -1; };
struct LocationId : ID<
	std::common_type_t<
		::algo::Train::TrainId::IdType,
		StationId::IdType
	>,
	LocationIdTag
> {
	static const IdType TRAIN_FLAG = ((IdType)(-1)) & ~(((IdType)(-1)) >> 1);

	LocationId(::algo::Train::TrainId tid) : ID(tid.getValue() | TRAIN_FLAG) { }
	LocationId(StationId sid) : ID(sid.getValue()) { }
	LocationId(IdType val) : ID(val) { }
	LocationId() : ID() { }

	bool isTrain() { return (getValue() & TRAIN_FLAG) != 0; }
	bool isStation() { return (getValue() & TRAIN_FLAG) == 0; }

	::algo::Train::TrainId asTrainId() {
		if (!isTrain()) { throw std::invalid_argument("Invalid Train id" + std::to_string(getValue())); }
		return make_id<::algo::Train::TrainId>(getValue() & (~TRAIN_FLAG));
	}
	StationId asStationId() {
		if (!isStation()) { throw std::invalid_argument("Invalid Station id" + std::to_string(getValue())); }
		return make_id<StationId>(getValue());
	}

	void print(std::ostream& os) {
		if (getValue() == DEFAULT_VALUE) {
			os << "<DEFAULT>";
		} else if (isTrain()) {
			os << 't' << asTrainId().getValue();
		} else if (isStation()) {
			os << 's' << asStationId().getValue();
		}
	}
};

inline std::ostream& operator<<(std::ostream& os, LocationId loc) {
	loc.print(os);
	return os;
}

int main() {
	auto tid_4 = make_id<::algo::Train::TrainId>(4);
	auto sid_5 = make_id<StationId>(5);

	auto lid_t4 = make_id<LocationId>(tid_4);
	auto lid_s5 = make_id<LocationId>(sid_5);

	std::cout << lid_t4 << '\n';
	std::cout << lid_s5 << '\n';
}
