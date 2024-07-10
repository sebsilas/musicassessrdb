import { db } from '../../core/rds'

export const handler = async (event) => {
    let result = {}
    try {
        result["songs"] = await db` SELECT song_name, STRING_AGG(item_id, '# ') AS phrases
        FROM item_bank_singpause
        GROUP BY song_name;` ?? []
        if (result["songs"].count > 0) {
            result["songs"].forEach((item, index) => {
                let phrases = item["phrases"].split("#"); result["songs"][index].phrases = phrases
            });
        }
    } catch (error) {
        console.error('Database connection error', error);
    }
    return {
        statusCode: 200,
        body: JSON.stringify(result
        ),
    };
};