#include <list>
#include <set>
#include <vector>
#include <cstddef>

namespace golvok::allocator {

class Allocator {
	struct Chunk {
		// wow...
		mutable std::byte* start;
		mutable std::size_t size;
		mutable bool is_free;
	};
	struct ChunkCompare {
		using is_transparent = std::true_type;
		bool operator()(const Chunk& lhs, const Chunk& rhs) const {
			return lhs.start < rhs.start;
		}
		bool operator()(const std::byte* lhs, const Chunk& rhs) const {
			return lhs < rhs.start;
		}
		bool operator()(const Chunk& lhs, const std::byte* rhs) const {
			return lhs.start < rhs;
		}
	};

	using ChunkList = std::set<Chunk, ChunkCompare>;
	using ChunkIter = typename ChunkList::iterator;

	struct Bin {
		std::size_t max_size;
		std::vector<ChunkIter> chunks;
	};

public:
	Allocator(std::byte* start, std::size_t size)
		: chunks{Chunk{start, size, true}}
		, bins{}
	{
		for (std::size_t bin_max_size = 1 << 5; bin_max_size != 1 << 10; bin_max_size <<= 1) {
			bins.push_back({bin_max_size, {}});
		}
		// create the biggest/fallback bin
		bins.push_back({std::numeric_limits<std::size_t>::max(), {}});

		returnChunk(chunks.begin()); // insert the initial chunk in the right place
	}

	std::byte* alloc(std::size_t size) {
		for (auto& bin : bins) {
			if (size > bin.max_size) continue; // definitely no big enough chunks

			// find in bin's chunks and remove it
			auto big_enough_and_free = [&](auto& chunk_it) { return chunk_it->size >= size && chunk_it->is_free; };
			auto bin_chunk_it = std::find_if(bin.chunks.begin(), bin.chunks.end(), big_enough_and_free);
			if (bin_chunk_it == bin.chunks.end()) continue; // no chunks big enough? go to next bin

			auto chunk_it = *bin_chunk_it;
			bin.chunks.erase(bin_chunk_it);

			// maybe split
			if (chunk_it->size > size) {
				auto unused_part = Chunk{chunk_it->start + size, chunk_it->size - size, true};
				returnChunk(chunks.insert(std::next(chunk_it), unused_part));
				chunk_it->size = size; // resize fonud chunk
			}

			// will always use this chunk
			chunk_it->is_free = false;
			return chunk_it->start;
		}
		std::terminate();
	}

	void free(std::byte* alloced_addr) {
		// find chunk
		auto chunk_it = findChunkForAddr(alloced_addr);
		if (chunk_it->start != alloced_addr) std::terminate();
		if (chunk_it->is_free) std::terminate();

		// maybe merge next into us
		{
			const auto next_chunk_it = std::next(chunk_it);
			if (next_chunk_it != chunks.end() && next_chunk_it->is_free) {
				unBinChunk(next_chunk_it);
				chunk_it->size += next_chunk_it->size; // we're not in a bin so no bin to update
				deleteChunk(next_chunk_it); // next is now redundant
			}
		}

		// maybe merge prev into us
		if (chunk_it != chunks.begin()) {
			const auto prev_chunk_it = std::prev(chunk_it);
			if (prev_chunk_it->is_free) {
				unBinChunk(prev_chunk_it);
				chunk_it->size += prev_chunk_it->size;
				chunk_it->start = prev_chunk_it->start;
				deleteChunk(prev_chunk_it);
			}
		}

		// we're the good chunk if we didn't merge into prev, so return us to the pool
		chunk_it->is_free = true;
		returnChunk(chunk_it);
	}

private:
	void unBinChunk(ChunkIter chunk_it) {
		auto& bin = binForSize(chunk_it->size);
		auto bin_chunk_it = std::find(bin.chunks.begin(), bin.chunks.end(), chunk_it);
		if (bin_chunk_it == bin.chunks.end()) std::terminate();
		bin.chunks.erase(bin_chunk_it);
	}
	void deleteChunk(ChunkIter chunk_it) {
		chunks.erase(chunk_it);
	}
	void returnChunk(ChunkIter chunk) {
		binForSize(chunk->size).chunks.push_back(chunk);
	}
	Bin& binForSize(std::size_t size) {
		for (auto& bin : bins) {
			if (size < bin.max_size) return bin;
		}
		std::terminate();
	}

	ChunkIter findChunkForAddr(std::byte* addr) {
		// if constexpr (chunk_list) {
		// 	auto at_least_addr = [&](auto& chunk) { return chunk.start >= alloced_addr; };
		// 	return std::find_if(chunks.begin(), chunks.end(), at_least_addr);
		// } else if constexpr (chunk_set) {
		return chunks.find(addr);
		// }
	}

	ChunkList chunks;
	std::vector<Bin> bins;
};

}